-module(markov).
-compile(export_all).

-include("definitions.hrl").

-define(OPTS, [global, {return, binary}]).

filter(Msg) ->
	M1 = util:bin_to_lower(Msg), % Lowercase
	M2 = re:replace(M1, "[\r\n'\"]+", "", ?OPTS), % Remove junk we don't care about
	M3 = re:replace(M2, "\\((.*?)\\)", "\\1", ?OPTS), % |
	M4 = re:replace(M3, "\\[(.*?)\\]", "\\1", ?OPTS), % | Remove *matched* brackets
	M5 = re:replace(M4, "\\{(.*?)\\}", "\\1", ?OPTS), % |
	M6 = re:replace(M5, "<(.*?)>",     "\\1", ?OPTS), % |
	M7 = re:replace(M6, "https?://\\S*", "", ?OPTS), % Remove URLs
	M8 = re:replace(M7, "\\.[\\.\\s]*\\.", "...", ?OPTS), % Replace strings of ..... and spaces with just three
	M9 = re:replace(M8, "(\\w)([!?.,])", "\\1 \\2", ?OPTS), % Move punctuation away from preceding word
	M10 = re:replace(M9, "([!?.,])(\\w)", "\\1 \\2", ?OPTS), % ....and the next word too
	M11 = re:replace(M10, "\s\s+", " ", ?OPTS), % Replace multiple spaces with just one

	binary:split(M11, <<" ">>, [global]). % Split on spaces

defilter(M0) ->
	M1 = re:replace(M0, "([\\w\\d]) ([!?.,])", "\\1\\2", ?OPTS), % Move punctuation back to preceding word

	M1.

initialise() ->
	Trees = config:get_value(data, [?MODULE]),
	spawn(fun() ->
			register(markov, self()),
			loop(Trees)
		end).

deinitialise() ->
	markov ! stop.

set(_, [], V) -> V;
set(D,[K|Ks],V) ->
	case orddict:find(K, D) of
		{ok,DV} -> orddict:store(K, set(DV, Ks, V), D);
		error -> orddict:store(K, set([], Ks, V), D)
	end.

get(D, Ks) -> get(D, Ks, '$none').
get(D, [], _) -> D;
get(D, [K|Ks], V) ->
	case orddict:find(K, D) of
		{ok, DV} -> get(DV, Ks, V);
		error -> V
	end.

inc(D, []) -> D+1;
inc(D, [K|Ks]) ->
	case orddict:find(K, D) of
		{ok, DV} -> orddict:store(K, inc(DV, Ks), D);
		error when Ks == [] -> orddict:store(K, 1, D);
		error -> orddict:store(K, inc([], Ks), D)
	end.

loop(Trees) ->
	receive
		stop ->
			config:set_value(data, [?MODULE], Trees);
		{Chan, M, Reply} ->
			case (catch case Reply of
				pinged -> reply(Trees, Chan, M), Trees;
				X ->
					T3 = lists:foldl(fun({A,B}, T0) ->
							T1 = inc(T0, [fwd, A, B]),
							inc(T1, [bwd, B, A])
						end, Trees, lists:zip([start | M], M ++ [finish])),
					case X of
						true -> reply(T3, Chan, M);
						_ -> ok
					end,
					T3
			end) of
				{'EXIT', T} ->
					io:fwrite("Err: ~p\n", [T]);
				T ->
					loop(T)
			end
	end.

handle_event(msg_nocommand, {#user{nick=_Nick}, Channel, Tokens}) ->
	M = filter(list_to_binary(string:join(Tokens, " "))),

	case hd(M) of
		<<"nt">> -> ok;
		<<"nanotrasen_inc">> -> ok;
		<<$!, _/binary>> -> ok;
		_ ->

			Nick = list_to_binary(string:to_lower(config:require_value(config, [bot, nick]))),
			NickS = <<Nick/binary, "s">>,

			X = case lists:member(Nick, M) orelse lists:member(NickS, M) orelse lists:member(<<"nt">>, M) orelse lists:member(<<"nts">>, M) orelse lists:member(<<"nti">>, M) orelse lists:member(<<"ntis">>, M) of
				true -> pinged;
				false -> random:uniform(100) =< config:get_value(config, [?MODULE, replyrate], 0)
			end,
			markov ! {Channel, M, X}
	end,
	ok;
handle_event(_, _) -> ok.

reply(T, Chan, Msg) ->
	Contexts = lists:zip(Msg, lists:map(fun(Word) ->
			lists:sum(lists:map(fun({_,V}) -> V end, get(T, [fwd, Word], []))) +
			lists:sum(lists:map(fun({_,V}) -> V end, get(T, [fwd, Word], [])))
		end, Msg)),
	UseContexts = lists:filter(fun({_,N}) -> N >= 3 end, Contexts),
	if
		UseContexts == [] -> ok; % no contexts to use!
		true ->
			SortedContexts = [{_,MinContexts}|_] = lists:sort(fun({_,A}, {_,B}) -> A =< B end, UseContexts),
			RareContexts = lists:takewhile(fun({_,C}) -> C == MinContexts end, SortedContexts),
			{Base,_} = util:simple_pick(RareContexts),
			ReplyTokens = build(T, [Base]),

			ReplyParts = lists:zip3(
				[start, start| ReplyTokens],
				[start| ReplyTokens] ++ [finish],
				ReplyTokens ++ [finish, finish]),
			Reply = lists:flatmap(fun({Before,This,After}) ->

					T0 = case lists:keyfind(This, 1, replace()) of % Apostrophe fixes
						{_, Replacement} -> Replacement;
						false -> This
					end,

					{T1,Final} = case After == finish andalso not lists:member(T0, [<<".">>, <<"?">>, <<"!">>]) of % Sentence should end with . ? or !
						true -> {T0, [<<".">>]};
						false -> {T0, []}
					end,

					T2 = case lists:member(Before, [<<".">>, <<"?">>, <<"!">>, start]) of % First word of sentence is capitalised
						true -> capitalise(T1);
						false -> T1
					end,

					[T2 | Final]
				end, lists:droplast(tl(ReplyParts))),

%			timer:sleep(1000 * random:uniform(length(Reply))),
			core ! {irc, {msg, {Chan, defilter(util:binary_join(Reply, <<" ">>))}}}
	end.

build(T, Lst) ->
	build_fwd(get(T,[fwd]), build_bwd(get(T,[bwd]), Lst)).

build_bwd(MD, [Wd|Rst]) ->
%	io:fwrite("Building backwards from ~p\n", [[Wd|Rst]]),
	case pick_word(orddict:fetch(Wd, MD), length(Rst)+11) of
		start -> [Wd|Rst];
		Word -> build_bwd(MD, [Word,Wd|Rst])
	end.

build_fwd(MD, Lst) ->
%	io:fwrite("Building forwards from ~p\n", [Lst]),
	Wd = lists:last(Lst),
	case pick_word(orddict:fetch(Wd, MD), length(Lst)) of
		finish -> Lst;
		Word -> build_fwd(MD, Lst ++ [Word])
	end.

pick_word(Opts, FinalBias) ->
	Total = lists:sum(lists:map(fun
			({finish,N}) -> N * FinalBias;
			({start, N}) -> N * FinalBias;
			({_, N}) -> N
		end, Opts)),
	Select = random:uniform(Total),
	pick_word0(Opts, FinalBias, Select).

pick_word0([{Opt,N}|___], FBias, Select) when (Opt == finish orelse Opt == start) andalso N*FBias >= Select -> Opt;
pick_word0([{Opt,N}|Rst], FBias, Select) when (Opt == finish orelse Opt == start) -> pick_word0(Rst, FBias, Select - N*FBias);
pick_word0([{Opt,N}|___], _____, Select) when N >= Select -> Opt;
pick_word0([{___,N}|Rst], FBias, Select) -> pick_word0(Rst, FBias, Select - N).

capitalise(<<A/utf8, B/binary>>) when $a =< A andalso A =< $z ->
	Capd = A + ($A-$a),
	<<Capd/utf8, B/binary>>;
capitalise(Bin) -> Bin.

replace() ->
	[
		{<<"i">>, <<"I">>},
		{<<"im">>, <<"I'm">>},
		{<<"youre">>, <<"you're">>},
		{<<"hes">>, <<"he's">>},
		{<<"shes">>, <<"she's">>},
		{<<"whos">>, <<"who's">>},
		{<<"wholl">>, <<"who'll">>},
		{<<"theyre">>, <<"they're">>},
		{<<"theyll">>, <<"they'll">>},
		{<<"thats">>, <<"that's">>},
		{<<"shouldntve">>, <<"shouldn't've">>},
		{<<"nt">>, <<"NT">>},
		{<<"nts">>, <<"NT's">>},
		{<<"nti">>, <<"NTI">>},
		{<<"ntis">>, <<"NTI's">>}
	].
