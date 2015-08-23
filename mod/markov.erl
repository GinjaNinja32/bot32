-module(markov).
-compile(export_all).

-include("definitions.hrl").
-record(mdata, {pairs, rchance=0}).

get_commands() ->
	[
		{"dumpmarkov", fun dmarkov/5, markov},
		{"markov", fun repmarkov/5, user},
		{"rmarkov", fun rawmarkov/5, user},
		{"check", fun check/5, admin},
		{"forget", fun forget/5, admin},
		{"rcheck", fun rcheck/5, admin},
		{"rforget", fun rforget/5, admin}
	].

dmarkov(_, RT, P, _, S) ->
	Data = get_data(S),
	Pairs = Data#mdata.pairs,
	common:debug("MARKOV", "~p", [Pairs]),
	{irc, {msg, {RT, [P, "Markov dumped."]}}}.

repmarkov(_, RT, P, [], _) -> {irc, {msg, {RT, [P, "Words!"]}}};
repmarkov(_, RT, P, W, S) ->
	Data = get_data(S),
	Reply = reply_to_markov(W, Data),
	{irc, {msg, {RT, [P, util:binary_join(Reply, <<" ">>)]}}}.

rawmarkov(_, RT, P, [], _) -> {irc, {msg, {RT, [P, "Words!"]}}};
rawmarkov(_, RT, P, W, S) ->
	Data = get_data(S),
	BinWords = lists:map(fun filter_word/1, W),
	Reply = build_from(BinWords, Data#mdata.pairs),
	{irc, {msg, {RT, [P, util:binary_join(Reply, <<" ">>)]}}}.


check(_, RT, P, W, _) when length(W) /= 1 -> {irc, {msg, {RT, [P, "One word to check."]}}};
check(_, RT, P, [LW], S) ->
	W = list_to_binary(LW),
	Data = get_data(S),
	Pairs = Data#mdata.pairs,
	NewPairs = orddict:filter(fun({A,B},_) -> A/=W andalso B/=W end, Pairs),
	{irc, {msg, {RT, [P, io_lib:format("~b contexts match.", [length(Pairs) - length(NewPairs)])]}}}.

forget(_, RT, P, W, _) when length(W) /= 1 -> {irc, {msg, {RT, [P, "One word to forget."]}}};
forget(_, RT, P, [LW], S) ->
	W = list_to_binary(LW),
	Data = get_data(S),
	Pairs = Data#mdata.pairs,
	NewPairs = orddict:filter(fun({A,B},_) -> A/=W andalso B/=W end, Pairs),
	core ! {irc, {msg, {RT, [P, io_lib:format("~b contexts forgotten.", [length(Pairs) - length(NewPairs)])]}}},
	{setkey, {?MODULE, Data#mdata{pairs=NewPairs}}}.


rcheck(_, RT, P, W, _) when length(W) /= 1 -> {irc, {msg, {RT, [P, "One regex to check."]}}};
rcheck(_, RT, P, [R], S) ->
	Data = get_data(S),
	Pairs = Data#mdata.pairs,
	NewPairs = orddict:filter(fun({A,B},_) ->
			(A == none orelse re:run(A, R, [{capture, none}]) /= match) andalso
			(B == none orelse re:run(B, R, [{capture, none}]) /= match)
		end, Pairs),
	{irc, {msg, {RT, [P, io_lib:format("~b contexts match.", [length(Pairs) - length(NewPairs)])]}}}.

rforget(_, RT, P, W, _) when length(W) /= 1 -> {irc, {msg, {RT, [P, "One regex to forget."]}}};
rforget(_, RT, P, [R], S) ->
	Data = get_data(S),
	Pairs = Data#mdata.pairs,
	NewPairs = orddict:filter(fun({A,B},_) ->
			(A == none orelse re:run(A, R, [{capture, none}]) /= match) andalso
			(B == none orelse re:run(B, R, [{capture, none}]) /= match)
		end, Pairs),
	core ! {irc, {msg, {RT, [P, io_lib:format("~b contexts forgotten.", [length(Pairs) - length(NewPairs)])]}}},
	{setkey, {?MODULE, Data#mdata{pairs=NewPairs}}}.


initialise(T) ->
	Data = load_data(),
	set_data(T, Data).

deinitialise(T) ->
	save_data(get_data(T)),
	T#state{moduledata=orddict:erase(markov, T#state.moduledata)}.

%

load_data() ->
	case file:consult("markov.crl") of
		{ok, [Pairs]} ->
			logging:log(info, "MARKOV", "Loaded."),
			#mdata{pairs=Pairs};
		{error, T} ->
			logging:log(info, "MARKOV", "Creating new (error is ~p)", [T]),
			#mdata{pairs=[]}
	end.

save_data(Data) ->
	T = file:write_file("markov.crl", io_lib:format("~p.~n", [Data#mdata.pairs])),
	logging:log(info, "MARKOV", "Save status: ~p", [T]).

get_data(#state{moduledata=M}) ->
	case orddict:find(markov, M) of
		{ok, Value} -> Value;
		error -> #mdata{pairs=[]}
	end.

set_data(S=#state{moduledata=M}, Data) ->
	S#state{moduledata=orddict:store(markov, Data, M)}.

handle_event(msg, {_, Channel, Msg}, S) ->
	case S#state.nick of
		Channel -> ok;
		_ ->
			Data = get_data(S),
			case Data#mdata.rchance+1 > random:uniform(100) of
				true ->
					Rpl = reply_to_markov(Msg, Data),
					core ! {irc, {msg, {Channel, util:binary_join(Rpl, <<" ">>)}}};
				false -> learn_from_markov(Msg, Data)
			end
	end;
handle_event(_,_,_) -> ok.

%

learn_from_markov(Msg, Data) ->
	TrueMsg = lists:map(fun filter_word/1, Msg),
%	common:debug("markov", "learning string ~s", [util:binary_join(TrueMsg, <<" ">>)]),
	PairsToAdd = lists:zip([none | TrueMsg], lists:append(TrueMsg, [none])),

	NewPairs = lists:foldl(fun(P, Prs) ->
		case orddict:find(P, Prs) of
			{ok, Value} -> orddict:store(P, Value+1, Prs);
			error -> orddict:store(P, 1, Prs)
		end end, Data#mdata.pairs, PairsToAdd),

	self() ! {setkey, {markov, Data#mdata{pairs=NewPairs}}},
	ok.

reply_to_markov(Msg, Data) ->
	TrueMsg = lists:map(fun filter_word/1, Msg),
%	common:debug("markov", "replying to ~s", [util:binary_join(TrueMsg, <<" ">>)]),
%	MsgPairs = lists:zip(lists:reverse(tl(lists:reverse(TrueMsg))), tl(TrueMsg)),

	case orddict:filter(fun({A,B}, _) -> lists:member(A, TrueMsg) orelse lists:member(B, TrueMsg) end, Data#mdata.pairs) of
		[] -> [<<"???">>];
		MatchingPairs ->
			{_,RarePairs} = orddict:fold(fun
					(__, Frq, {MinFrq, PrList}) when Frq >  MinFrq -> {MinFrq, PrList};
					(Pr, Frq, {MinFrq, PrList}) when Frq == MinFrq -> {MinFrq, [Pr | PrList]};
					(Pr, Frq, {MinFrq, ______}) when Frq <  MinFrq -> {   Frq, [Pr]}
				end, {large, []}, MatchingPairs),
			[PUA, PUB] = tuple_to_list(lists:nth(random:uniform(length(RarePairs)), RarePairs)),
			case lists:map(fun(T) -> lists:member(T, TrueMsg) end, [PUA, PUB]) of
				[true, true] -> build_from([lists:nth(random:uniform(2), [PUA, PUB])], Data#mdata.pairs);
				[true, false] -> build_from([PUA], Data#mdata.pairs);
				[false, true] -> build_from([PUB], Data#mdata.pairs);
				_ -> <<"??? (err)">>
			end
	end.

build_from(Words, P) ->
	build_backwards(build_forwards(Words, P), P).

build_backwards(Words, _) when length(Words) >= 20 -> Words;
build_backwards(Words, P) ->
	FirstWord = hd(Words),
	case orddict:filter(fun ({_,K},_) when K == FirstWord -> true;
				(_,_) -> false
			end, P) of
		[] -> Words;
		Pairs ->
			N = random:uniform(orddict:fold(fun(_,X,Y)->X+Y end, 0, Pairs)),
			try
				orddict:fold(fun
						(T,X,Y) when X+Y >= N -> throw(T);
						(_,X,Y) -> X+Y
					end, 0, Pairs)
			catch
				throw:{none,_} -> Words;
				throw:{Prv,_} ->
					case lists:member(binary:at(Prv, byte_size(Prv)-1), sentence_end()) of
						true -> Words;
						false -> build_backwards(lists:append([Prv], Words), P)
					end
			end
	end.

build_forwards(Words, _) when length(Words) >= 15 -> Words;
build_forwards(Words, P) ->
	LastWord = hd(lists:reverse(Words)),
	case lists:member(binary:at(LastWord, byte_size(LastWord)-1), sentence_end()) of
		true -> Words;
		false ->
			case orddict:filter(fun	({K,_},_) when K == LastWord -> true;
						(_,_) -> false
					end, P) of
				[] -> Words;
				Pairs ->
					N = random:uniform(orddict:fold(fun(_,X,Y)->X+Y end, 0, Pairs)),
					try
						orddict:fold(fun
								(T,X,Y) when X+Y >= N -> throw(T);
								(_,X,Y) -> X+Y
							end, 0, Pairs)
					catch
						throw:{_,none} -> Words;
						throw:{_,Nxt} -> build_forwards(lists:append(Words, [Nxt]), P)
					end
			end
	end.


sentence_end() ->
	[$!, $?, $.].

filter_word(Word) ->
	lists:foldl(fun(Filter, Wd) ->
			R = util:regex_escape(Filter),
			re:replace(Wd, R, <<"">>, [{return, binary}, global])
		end, Word,
			[<<"[">>,<<"]">>,<<"{">>,<<"}">>,<<"(">>,<<")">>,<<"<">>,<<">">>,<<"\"">>,<<"'">>,<<"*">>,<<"\t">>]).
