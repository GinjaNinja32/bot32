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

handle_event(msg_nocommand, {#user{nick=_Nick}, Channel, Tokens}) ->
	M = filter(list_to_binary(string:join(Tokens, " "))),


	Nick = list_to_binary(string:to_lower(config:require_value(config, [bot, nick]))),
	NickS = <<Nick/binary, "s">>,

	case case lists:member(Nick, M) orelse lists:member(NickS, M) orelse lists:member(<<"nt">>, M) orelse lists:member(<<"nts">>, M) of
		true -> pinged;
		false -> random:uniform(100) =< config:get_value(config, [?MODULE, replyrate], 0)
	end of
		pinged -> reply(Channel, M);
		T ->
			lists:foreach(fun({A,B}) ->
					config:mod_get_value(data, [?MODULE, fwd, A, B], fun('$none') -> 1; (T) -> T+1 end),
					config:mod_get_value(data, [?MODULE, bwd, B, A], fun('$none') -> 1; (T) -> T+1 end)
				end, lists:zip([start | M], M ++ [finish])),
			case T of
				true -> reply(Channel, M);
				_ -> ok
			end
	end,
	ok;
handle_event(_, _) -> ok.

reply(Chan, Msg) ->
	Contexts = lists:zip(Msg, lists:map(fun(Word) ->
			lists:sum(lists:map(fun({_,V}) -> V end, config:get_value(data, [?MODULE, fwd, Word], []))) +
			lists:sum(lists:map(fun({_,V}) -> V end, config:get_value(data, [?MODULE, bwd, Word], [])))
		end, Msg)),
	UseContexts = lists:filter(fun({_,N}) -> N >= 3 end, Contexts),
	if
		UseContexts == [] -> ok; % no contexts to use!
		true ->
			SortedContexts = [{_,MinContexts}|_] = lists:sort(fun({_,A}, {_,B}) -> A =< B end, UseContexts),
			RareContexts = lists:takewhile(fun({_,T}) -> T == MinContexts end, SortedContexts),
			{Base,_} = util:simple_pick(RareContexts),
			ReplyTokens = build([Base]),

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

			core ! {irc, {msg, {Chan, defilter(util:binary_join(Reply, <<" ">>))}}}
	end.

build(Lst) ->
	FWD = config:get_value(data, [?MODULE, fwd]),
	BWD = config:get_value(data, [?MODULE, bwd]),
	build_fwd(FWD, build_bwd(BWD, Lst)).

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
		{<<"im">>, <<"i'm">>},
		{<<"youre">>, <<"you're">>},
		{<<"hes">>, <<"he's">>},
		{<<"shes">>, <<"she's">>}
	].
