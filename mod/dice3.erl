-module(dice3).
-compile(export_all).

-include("colordefs.hrl").

get_aliases() ->
	[
		{{"dice",["d"]}, ["rtd"]},
		{"dice", ["roll"]},
		{"edice", ["eroll", "exdice", "exroll"]}
	].

get_commands() ->
	[
		{"dice", fun dice/1, user},
		{"edice", fun edice/1, user},
		{"gurps", fun gurps/1, user},
		{"srun", fun srun/1, user},
		{"sredge", fun sredge/1, user},
		{"fate", fun fate/1, user}
	].

% utils

parse_params([[$@|Nicks] | Params]) -> {string:tokens(Nicks, ":/|"), Params};
parse_params(Params) -> {[], Params}.

send_to_all(RT, P, Nick, Others, DiceString, Result) ->
	core ! {irc, {msg, {RT, [P, Result]}}},
	lists:foreach(fun(T) ->
		core ! {irc, {msg, {T, [Nick, " has rolled ", DiceString, ": ", Result]}}}
	end, Others).

% GURPS

gurps(#{reply:=RT,ping:=P,nick:=Nick,params:=OriginalParams}) ->
	case parse_params(OriginalParams) of
		{N, []} ->
			Result = dice3("3d6", true),
			send_to_all(RT, P, Nick, N, "3d6", Result);
		{N, Params} ->
			T = list_to_integer(hd(Params)),
			Comment = case string:join(tl(Params), " ") of
				[] -> [];
				X -> [X,$:,$ ]
			end,
			Targets = if
				T >= 16 -> [6, 16,   17];
				T == 15 -> [5, 15,   16];
				T >=  7 -> [4,  T,   17];
				true    -> [4,  T, T+10]
			end,
			Formatter = fun
				([false,false,false]) -> [Comment, "\x036\x02Critical Failure!\x03\x02"];
				([false,false, true]) -> [Comment, "\x035\x02Failure!\x03\x02"];
				([false, true, true]) -> [Comment, "\x033\x02Success!\x03\x02"];
				([ true, true, true]) -> [Comment, "\x0310\x02Critical Success!\x03\x02"]
			end,
			Result = dice3(lists:flatten(io_lib:format("3d6<=~w", [Targets])), true, Formatter),
			send_to_all(RT, P, Nick, N, io_lib:format("a GURPS test at skill ~b", [T]), Result)
	end.

% SHADOWRUN

srun(#{reply:=RT,ping:=P,nick:=Nick,params:=OriginalParams}) ->
	{Nicks, Params} = parse_params(OriginalParams),
	T = list_to_integer(hd(Params)),
	{_, Dice} = rollraw(T, 6),
	{One,FivePlus} = lists:foldl(fun(1, {O,F}) -> {O+1,F}; (N, {O,F}) when N >= 5 -> {O, F+1}; (_, D) -> D end, {0,0}, Dice),
	Summary = io_lib:format("~b failure~s, ~b hit~s", [One, util:s(One), FivePlus, util:s(FivePlus)]),
	Reply = if
		2*One >= T andalso FivePlus == 0 -> [Summary | ": Critical Glitch!"];
		2*One >= T -> [Summary | ": Glitch!"];
		true -> Summary
	end,
	send_to_all(RT, P, Nick, Nicks, io_lib:format("a Shadowrun test with ~b dice", [T]), Reply).
sredge(#{reply:=RT,ping:=P,nick:=Nick,params:=OriginalParams}) ->
	{Nicks, Params} = parse_params(OriginalParams),
	T = list_to_integer(hd(Params)),
	{ExtraDice, Dice} = get_sr_edge(T, 0, []),
	{One,FivePlus} = lists:foldl(fun(1, {O,F}) -> {O+1,F}; (N, {O,F}) when N >= 5 -> {O, F+1}; (_, D) -> D end, {0,0}, Dice),
	Summary = io_lib:format("~b failure~s, ~b hit~s (rolled ~b six~s)", [One, util:s(One), FivePlus, util:s(FivePlus), ExtraDice, util:s(ExtraDice, "es")]),
	Reply = if
		2*One >= T andalso FivePlus == 0 -> [Summary | ": Critical Glitch!"];
		2*One >= T -> [Summary | ": Glitch!"];
		true -> Summary
	end,
	send_to_all(RT, P, Nick, Nicks, io_lib:format("a Shadowrun edge test with ~b dice", [T]), Reply).

get_sr_edge(0, Sixes, D) -> {Sixes, D};
get_sr_edge(N, Sixes, D) ->
	{_, NewDice} = rollraw(N, 6),
	S = util:count(fun(T) -> T == 6 end, NewDice),
	get_sr_edge(S, S+Sixes, NewDice ++ D).

% FATE

fate(#{reply:=RT, ping:=P,nick:=Nick,params:=OriginalParams}) ->
	{Nicks, Params} = parse_params(OriginalParams),
	N = case Params of
		[] -> 0;
		[NS] -> list_to_integer(NS)
	end,
	{_, RawDice} = rollraw(4, 3),
	Dice = lists:map(fun(T) -> T-2 end, RawDice), % d3-2 is equivalent to a single FATE die
	Str = lists:map(fun(-1) -> ?LRED ++ "-" ++ ?RESET;
	                   ( 0) -> ?WHITE ++ ?BOLD ++ ?BOLD ++ "0" ++ ?RESET;
	                   (+1) -> ?LGREEN ++ "+" ++ ?RESET
		end, Dice),
	Total = lists:foldl(fun erlang:'+'/2, 0, Dice),
	Reply = io_lib:format("[~s|~s] ~s", [Str, show(Total), show(Total+N)]),
	send_to_all(RT, P, Nick, Nicks, io_lib:format("a Fate test at ~s~b", [if N >= 0 -> "+"; true -> "" end, N]), Reply).

show(N) when N < 0 -> io_lib:format("~s~b~s", [?LRED, N, ?RESET]);
show(N) when N > 0 -> io_lib:format("~s+~b~s", [?LGREEN, N, ?RESET]);
show(0) -> ?WHITE ++ "+0" ++ ?RESET.

% GENERIC

dice(X) -> dice(X, false).
edice(X) -> dice(X, true).

dice (#{reply:=RT,ping:=P,nick:=Nick,params:=OriginalParams}, Expand) ->
	{Nicks, Params} = parse_params(OriginalParams),
	DiceString = string:join(Params, " "),
	Reply = dice3(DiceString, Expand),
	send_to_all(RT, P, Nick, Nicks, DiceString, Reply).

dice3(String, Expand) -> dice3(String, Expand, fun(T) -> io_lib:format("~w", [T]) end).
dice3(String, Expand, Formatter) ->
	case tokenise(String, []) of
		error -> "Tokenisation error.";
		Tokens ->
			case parse(lists:reverse(Tokens)) of
				error -> "Parse error.";
				Expressions ->
					{S,V} = evaluate(Expressions, Expand),
					io_lib:format("~s : ~s~n", [Formatter(V),S])
			end
	end.

% TOKENISATION

tokens2() ->
	[{">=", '>='}, {"<=", '<='},
	 {"=<", '<='}, {"=>", '>='}].

tokens1() ->
	[{" ", '$none'}, {"	", '$none'},
	 {">", '>'}, {"<", '<'}, {"=", '='},
	 {"c", 'c'}, {"f", 'f'}, {"#", '#'}, {"!", '!'},
	 {"+", '+'}, {"-", '-'}, {"*", '*'}, {"/", '/'},
	 {"(", '('}, {")", ')'}, {"d", 'd'}, {"s", 's'}].

tokenise([], T) -> T;
tokenise([A|R], T) when $0 =< A andalso A =< $9 ->
	{Num,Rest} = lists:splitwith(fun(X) -> $0 =< X andalso X =< $9 end, [A|R]),
	tokenise(Rest, [list_to_integer(Num) | T]);
tokenise([$[|R], T) ->
	{Lst, [_|Rest]} = lists:splitwith(fun(X) -> X /= $] end, R),
	NumStrings = string:tokens(Lst, ", "),
	case catch lists:map(fun(Z) ->
				case list_to_integer(Z) of X when is_integer(X) -> X; _ -> throw(error) end
			end, NumStrings) of
		error -> error;
		NumList ->
			tokenise(Rest, [NumList|T])
	end;
tokenise([A,B|R], T) ->
	case lists:keyfind([A,B], 1, tokens2()) of
		{_, '$none'} -> tokenise(R, T);
		{_, X} -> tokenise(R, [X|T]);
		false ->
			case lists:keyfind([A], 1, tokens1()) of
				{_, '$none'} -> tokenise([B|R], T);
				{_, X} -> tokenise([B|R], [X|T]);
				false ->
					io:fwrite("~s~n", [[A,B|R]]),
					error
			end
	end;
tokenise([A], T) ->
	case lists:keyfind([A], 1, tokens1()) of
		{_, X} -> [X | T];
		false -> error
	end.

% PARSING

parse(Tokens) ->
	case a(Tokens) of
		{ok,Lst} ->
			case parseExpr(Lst) of
				[Tree] -> Tree;
				T when is_list(T) -> error;
				_ -> error
			end;
		mismatch -> error
	end.

parseExpr(Expr) when is_list(Expr) ->
	Ex = lists:map(fun parseExpr/1, Expr),
%	E2 = lists:map(fun([T]) -> T; (T) when not is_list(T) -> T; (_)  -> throw(error) end, Ex),
	parseExpression(Ex);
parseExpr(T) -> T.

parseExpression(Lst) ->
	Priority = [['d'], ['*','/'], ['+','-'], ['>','>=','<=','<','=']],
	lists:foldl(fun parseLeftAssoc/2, Lst, Priority).

parseLeftAssoc(_, error) -> error;
parseLeftAssoc(OpList, Toks) ->
	case lists:splitwith(fun(T) -> not lists:member(T, OpList) end, Toks) of
		{X, []} -> X;
		{X, [Op|Y]} ->
			LastTailX = if X /= [] -> util:lasttail(X); true -> none end,
			{TrueLeft, LeftRest} = if
				X == [] -> {default_left(Op), X};
				true ->
					case is_acceptable_left(Op, LastTailX) of
						true -> {LastTailX, util:droplast(X)};
						false -> {default_left(Op), X}
					end
			end,
			{TrueRight, RightRest} = if
				Y == [] -> {default_right(Op), Y};
				true ->
					case is_acceptable_right(Op, hd(Y), TrueLeft) of
						true -> {hd(Y), tl(Y)};
						false -> {default_right(Op), Y}
					end
			end,
			if
				TrueLeft == error orelse TrueRight == error -> error;
				true -> parseLeftAssoc(OpList, LeftRest ++ [{Op, TrueLeft, TrueRight}] ++ RightRest)
			end
			%parseLeftAssoc(OpList, util:droplast(X) ++ [{Op, util:lasttail(X), hd(Y)}] ++ tl(Y))
	end.


%is_acceptable_left(Op, X) when is_list(X) -> lists:member(Op, ['>', '>=', '<', '<=', '==']);
is_acceptable_left(_, X) -> is_tuple(X) orelse is_number(X) orelse is_list(X).

%is_acceptable_right(_, X, Y) when is_list(X) andalso is_list(Y) -> false;
%is_acceptable_right(Op, X, _) when is_list(X) andalso length(X) -> lists:member(Op, ['>', '>=', '<', '<=', '==']);
is_acceptable_right(_, X, _) -> is_tuple(X) orelse is_number(X) orelse is_list(X).

default_left('d') -> 1;
default_left(_) -> error.

default_right('d') -> 6;
default_right(_) -> error.

a(Lst) ->
	case catch b(Lst,0) of
		{L,[]} -> {ok,L};
		mismatch -> error
	end.

b([],_) -> {[],[]};
b(Lst, Lvl) ->
	case hd(Lst) of
		')' when Lvl == 0 -> throw(mismatch);
		')' ->  {[],tl(Lst)};
		'(' -> {I,E} = b(tl(Lst), Lvl+1), {I2,E2} = b(E,Lvl), {[I|I2], E2};
		T -> {I,E} = b(tl(Lst), Lvl), {[T|I],E}
	end.

% EVALUATION

comparison(AS, BS, AV, BV, OS, OV) when not is_list(AV) andalso not is_list(BV) -> % normal comparison
	Result = OV(AV, BV),
	Color = case Result of
		true -> "3";
		false -> "5"
	end,
	{[AS,3,Color,2,OS,3,2,BS], Result};
comparison(__, BS, AV, BV, OS, OV) when is_list(AV) ->
	Result = lists:map(fun(A) -> {A,OV(A, BV)} end, AV),
	Color = lists:map(fun({_,true}) -> "3"; ({_,false}) -> "5" end, Result),
	{[$[,32,lists:flatmap(fun({{N,_},C}) -> [3,C,2,integer_to_list(N),3,2,32] end, lists:zip(Result, Color)),$],OS,BS], lists:map(fun({_,A}) -> A end, Result)};
comparison(AS, __, AV, BV, OS, OV) when is_list(BV) ->
	Result = lists:map(fun(B) -> {B,OV(AV, B)} end, BV),
	Color = lists:map(fun({_,true}) -> "3"; ({_,false}) -> "5" end, Result),
	{[AS,OS,$[,32,lists:flatmap(fun({{N,_},C}) -> [3,C,2,integer_to_list(N),3,2,32] end, lists:zip(Result, Color)),$]], lists:map(fun({_,B}) -> B end, Result)}.

evaluate(Tree, Expand) ->
	case Tree of
		{Op, A, B} ->
			{AS,AV} = evaluate(A, Expand),
			{BS,BV} = evaluate(B, Expand),
			case Op of
				'+'  when (not is_list(AV)) andalso (not is_list(BV)) -> {[AS,$+,   BS], AV+ BV};
				'-'  when (not is_list(AV)) andalso (not is_list(BV)) -> {[AS,$-,   BS], AV- BV};
				'*'  when (not is_list(AV)) andalso (not is_list(BV)) -> {[AS,$*,   BS], AV* BV};
				'/'  when (not is_list(AV)) andalso (not is_list(BV)) -> {[AS,$/,   BS], AV/ BV};
				'<'  when (not is_list(AV)) orelse  (not is_list(BV)) -> comparison(AS, BS, AV, BV, "<", fun erlang:'<'/2);
				'<=' when (not is_list(AV)) orelse  (not is_list(BV)) -> comparison(AS, BS, AV, BV, "<=", fun erlang:'=<'/2);
				'>'  when (not is_list(AV)) orelse  (not is_list(BV)) -> comparison(AS, BS, AV, BV, ">", fun erlang:'>'/2);
				'>=' when (not is_list(AV)) orelse  (not is_list(BV)) -> comparison(AS, BS, AV, BV, ">=", fun erlang:'>='/2);
				'='  when (not is_list(AV)) orelse  (not is_list(BV)) -> comparison(AS, BS, AV, BV, "=", fun erlang:'=='/2);
				'd'  when (not is_list(AV)) andalso (not is_list(BV)) ->
					{Stat, Dice, Total} = roll(AV,BV),
					Display = if
						Expand andalso AV =< 10 -> io_lib:format("~w=~b", [Dice,Total]);
						true -> integer_to_list(Total)
					end,
					Str = case Stat of
						ok -> [$[,AS,$d,BS,$=,2,Display,2,$]];
						X -> [$[,X,AS,$d,BS,$=,2,Display,2,$]]
					end,
					{Str, Total};
				_ -> {"{invalid}", 0}
			end;
		[XTree] ->
			{S,V} = evaluate(XTree, Expand),
			{[$(,S,$)],V};
		T when is_list(T) -> {io_lib:format("~p", [T]), T};
		T -> {integer_to_list(T), T}
	end.

roll(N,_) when N > 1000000 -> {"\x035\x02X\x03\x02 ", [], 0};
roll(_,M) when M > 1000000 -> {"\x035\x02X\x03\x02 ", [], 0};
roll(N,M) ->
	case rollraw(N,M) of
		{Stat, Dice} when is_list(Dice) -> {Stat, Dice, lists:foldl(fun erlang:'+'/2, 0, Dice)};
		{Stat, Total} -> {Stat, [], Total}
	end.

rollraw(N, 1) -> {ok, lists:duplicate(N, 1)};
rollraw(N, M) ->
	case config:get_value(config, [?MODULE, dicemode]) of
		random when N > 100 -> {"\x038!\x03 ", roll_total(N, M)};
		random when M =< 100 -> {ok, get_n_m(N, M)};
		_ when N > 10 -> {ok, roll_total(N, M)};
		_ -> {ok, lists:map(fun(_) -> random:uniform(M) end, lists:duplicate(N, x))}
	end.

roll_total(N, M) ->
	io:fwrite("recursively rolling ~bd~b\n", [N, M]),
	Total = roll_total(N, M, 0),
	io:fwrite("rolled ~b\n", [Total]),
	Total.

roll_total(0, _, Total) -> Total;
roll_total(N, M, Total) -> roll_total(N-1, M, Total + random:uniform(M)).

get_n_m(N, M) ->
	case get_avail(M) of
		List when length(List) >= N ->
			common:debug("dice-cache", "using ~b of ~b available d~bs", [N, length(List), M]),
			{A,B} = lists:split(N, List),
			set_avail(M, B),
			A;
		List when length(List) =< N ->
			common:debug("dice-cache", "request for ~b of ~b available d~bs, fetching 100 more", [N, length(List), M]),
			NewHundred = random_org:generate(100, 1, M),
			NewList = List ++ NewHundred,
			{A,B} = lists:split(N, NewList),
			set_avail(M, B),
			A
	end.

get_avail(M) ->
	case get(dice_avail) of
		undefined -> [];
		Dict ->
			case orddict:find(M, Dict) of
				{ok, List} -> List;
				error -> []
			end
	end.

set_avail(M, List) ->
	case get(dice_avail) of
		undefined -> put(dice_avail, [{M, List}]);
		Dict -> put(dice_avail, orddict:store(M, List, Dict))
	end.











































