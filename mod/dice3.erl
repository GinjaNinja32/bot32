-module(dice3).
-compile(export_all).

-include("colordefs.hrl").

get_commands() ->
	[
		{"dice", fun dice/1, user},
		{"edice", fun edice/1, user},
		{"gurps", fun gurps/1, user},
		{"srun", fun srun/1, user},
		{"sredge", fun sredge/1, user},
		{"fate", fun fate/1, user},
		{"rtd", fun rtd/1, user}
		{"apoc", fun apoc/1, user}
	].

get_help("dice") ->
	[
		"'dice XdY' for a basic dice roll, using +, -, *, / to modify the value.",
		"Comparisons can be performed using >, >=, <, <=, and =.",
		"Comparisons involving lists (e.g. '[1, 2, 3]' or '[1 2 3]') will compare elementwise, and color-code accordingly.",
		"Use 'edice' for an exploded roll, showing the individual dice as well as the total.",
		"Use 'h', 'H', 'l', or 'L' to discard dice (e.g. '4d6H3' to roll 4d6 and keep the highest 3, or '5d10l2' to roll 5d10 and discard the lowest 2); the uppercase forms of these keep N dice, the lowercase discard; L/l from the lower end, H/h from the upper.",
		"Use '#' to perform the same roll multiple times (e.g. 4#3d6 to roll 3d6, four times)."
	];
get_help("edice") -> get_help("dice");
get_help(_) -> unhandled.

% utils

nicks(Sel) ->
	string:tokens(Sel, ":/|").

send_to_all(RT, P, Nick, Others, DiceString, Result) ->
	case lists:member("-", Others) of
		false -> core ! {irc, {msg, {RT, [P, Result]}}};
		true -> ok
	end,
	lists:foreach(fun
		("-") -> ok;
		(T) -> core ! {irc, {msg, {T, [Nick, " has rolled ", DiceString, ": ", Result]}}}
	end, Others).

% GURPS

gurps(#{reply:=RT, ping:=P, nick:=Nick, params:=Params, selector:=Selector}) ->
	case Params of
		[] ->
			Result = dice3("3d6", true),
			send_to_all(RT, P, Nick, nicks(Selector), "3d6", Result);
		_ ->
			T = list_to_integer(hd(Params)),
			Comment = case string:join(tl(Params), " ") of
				[] -> [];
				X -> [X,$:,$ ]
			end,
			Targets = if
				T >= 16 -> [6, 16,   17];
				T == 15 -> [5, 15,   16];
				T >=  7 -> [4,  T,   16];
				true    -> [4,  T, T+10]
			end,
			Formatter = fun
				([false,false,false]) -> [Comment, "\x036\x02Critical Failure!\x03\x02"];
				([false,false, true]) -> [Comment, "\x035\x02Failure!\x03\x02"];
				([false, true, true]) -> [Comment, "\x033\x02Success!\x03\x02"];
				([ true, true, true]) -> [Comment, "\x0310\x02Critical Success!\x03\x02"]
			end,
			Result = dice3(lists:flatten(io_lib:format("3d6<=~w", [Targets])), true, Formatter),
			send_to_all(RT, P, Nick, nicks(Selector), io_lib:format("a GURPS test at skill ~b", [T]), Result)
	end.

% SHADOWRUN

srun(#{reply:=RT,ping:=P,nick:=Nick,params:=Params,selector:=Selector}) ->
	T = list_to_integer(hd(Params)),
	{_, Dice} = rollraw(T, 6),
	{One,FivePlus} = lists:foldl(fun(1, {O,F}) -> {O+1,F}; (N, {O,F}) when N >= 5 -> {O, F+1}; (_, D) -> D end, {0,0}, Dice),
	Summary = io_lib:format("~b failure~s, ~b hit~s", [One, util:s(One), FivePlus, util:s(FivePlus)]),
	Reply = if
		2*One >= T andalso FivePlus == 0 -> [Summary | ": Critical Glitch!"];
		2*One >= T -> [Summary | ": Glitch!"];
		true -> Summary
	end,
	send_to_all(RT, P, Nick, nicks(Selector), io_lib:format("a Shadowrun test with ~b dice", [T]), Reply).
sredge(#{reply:=RT,ping:=P,nick:=Nick,params:=Params, selector:=Selector}) ->
	T = list_to_integer(hd(Params)),
	{ExtraDice, Dice} = get_sr_edge(T, 0, []),
	{One,FivePlus} = lists:foldl(fun(1, {O,F}) -> {O+1,F}; (N, {O,F}) when N >= 5 -> {O, F+1}; (_, D) -> D end, {0,0}, Dice),
	Summary = io_lib:format("~b failure~s, ~b hit~s (rolled ~b six~s)", [One, util:s(One), FivePlus, util:s(FivePlus), ExtraDice, util:s(ExtraDice, "es")]),
	Reply = if
		2*One >= T andalso FivePlus == 0 -> [Summary | ": Critical Glitch!"];
		2*One >= T -> [Summary | ": Glitch!"];
		true -> Summary
	end,
	send_to_all(RT, P, Nick, nicks(Selector), io_lib:format("a Shadowrun edge test with ~b dice", [T]), Reply).

get_sr_edge(0, Sixes, D) -> {Sixes, D};
get_sr_edge(N, Sixes, D) ->
	{_, NewDice} = rollraw(N, 6),
	S = util:count(fun(T) -> T == 6 end, NewDice),
	get_sr_edge(S, S+Sixes, NewDice ++ D).

% FATE

fate(#{reply:=RT, ping:=P,nick:=Nick,params:=Params, selector:=Selector}) ->
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
	send_to_all(RT, P, Nick, nicks(Selector), io_lib:format("a Fate test at ~s~b", [if N >= 0 -> "+"; true -> "" end, N]), Reply).

show(N) when N < 0 -> io_lib:format("~s~b~s", [?LRED, N, ?RESET]);
show(N) when N > 0 -> io_lib:format("~s+~b~s", [?LGREEN, N, ?RESET]);
show(0) -> ?WHITE ++ "+0" ++ ?RESET.

% RTD

rtd(#{reply:=Reply, ping:=Ping, nick:=Nick, selector:=Selector, params:=Params}) ->
	Comment = case string:join(Params, " ") of
		[] -> [];
		X -> [X,$:,$ ]
	end,
	RTD = [
		"\x036\x02Critical Failure!\x03\x02",
		"\x035\x02Failure!\x03\x02",
		"\x02Partial Success!\x02",
		"\x033\x02Success!\x03\x02",
		"\x039\x02Perfect!\x03\x02",
		"\x0310\x02Overkill!\x03\x02"
	],
	Result = dice3("d6", true, fun([T]) -> [Comment, lists:nth(T, RTD)] end),
	send_to_all(Reply, Ping, Nick, nicks(Selector), "an RTD roll", Result).

% APOCALYPSE WORLD

apoc(#{reply:=RT, ping:=P, nick:=Nick, params:=Params, selector:=Selector}) ->
	case Params of
		[] ->
			M = 0,
			Comment = "";
		_ ->
			M = list_to_integer(hd(Params)),
			Comment = case string:join(tl(Params), " ") of
				[] -> [];
				X -> [X,$:,$ ]
			end,
	end,
	Formatter = fun
		([false,false,false]) -> [Comment, "\x034\x02Miss!\x03\x02"];
		([false,false, true]) -> [Comment, "\x038\x02Weak Hit!\x03\x02"];
		([false, true, true]) -> [Comment, "\x039\x02Strong Hit!\x03\x02"];
		([ true, true, true]) -> [Comment, "\x0311\x02Strong Hit!\x03\x02"]
	end,
	Result = dice3(lists:flatten(io_lib:format("2d6+0~w>=[7, 10, 12]", [M])), true, Formatter),
	send_to_all(RT, P, Nick, nicks(Selector), io_lib:format("a PBtA roll at mod ~b", [M]), Result).

% GENERIC

dice(X) -> dice(X, false).
edice(X) -> dice(X, true).

dice (#{reply:=RT,ping:=P,nick:=Nick,params:=Params, selector:=Selector}, Expand) ->
	DiceString = string:join(Params, " "),
	Reply = dice3(DiceString, Expand),
	send_to_all(RT, P, Nick, nicks(Selector), DiceString, Reply).

dice3(String, Expand) -> dice3(String, Expand, fun(T) -> io_lib:format("~w", [T]) end).
dice3(String, Expand, Formatter) ->
%	io:fwrite("tokenising ~p\n", [String]),
	case tokenise(String, []) of
		error -> "Tokenisation error.";
		{Tokens, Comment} ->
%			io:fwrite("parsing ~p\n", [lists:reverse(Tokens)]),
			case parse(lists:reverse(Tokens)) of
				error -> "Parse error.";
				Expressions ->
%					io:fwrite("evaluating ~p\n", [Expressions]),
					{S,V} = evaluate(Expressions, Expand),
					io_lib:format("~s~s : ~s~n", [Comment, Formatter(V),S])
			end
	end.

% TOKENISATION

tokens2() ->
	[{">=", '>='}, {"<=", '<='},
	 {"=<", '<='}, {"=>", '>='}].

tokens1() ->
	[{" ", '$none'}, {"	", '$none'}, {"%", '%'},
	 {">", '>'}, {"<", '<'}, {"=", '='}, {"!", '!'},
	 {"+", '+'}, {"-", '-'}, {"*", '*'}, {"/", '/'},
	 {"(", '('}, {")", ')'}, {"d", 'd'}, {"#", '#'},
	 {"h", 'h'}, {"H", 'H'}, {"l", 'l'}, {"L", 'L'},
	 {"s", 's'}, {"i", 'i'}, {"c", 'c'}
	].

tokenise([$;|Rst], T) -> {T, [string:strip(Rst), ": "]};
tokenise([$:|Rst], T) -> {T, [string:strip(Rst), ": "]};
tokenise([$||Rst], T) -> {T, [string:strip(Rst), ": "]};
tokenise([], T) -> {T, ""};
tokenise([A|R], T) when $0 =< A andalso A =< $9 ->
	{Num,Rest} = lists:splitwith(fun(X) -> $0 =< X andalso X =< $9 end, [A|R]),
	tokenise(Rest, [list_to_integer(Num) | T]);
tokenise([$[|R], T) ->
	{RLst, [_|Rest]} = lists:splitwith(fun(X) -> X /= $] end, R),
	Lst = string:tokens(RLst, ","),
	case catch lists:map(fun(Z) ->
				case tokenise(Z, []) of
					error -> error;
					{Tokens, _} -> parse(lists:reverse(Tokens))
				end
			end, Lst) of
		error -> error;
		Exprs -> tokenise(Rest, [{list,Exprs}|T])
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
		{_, X} -> {[X | T], ""};
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
	Priority = [['d', 'i'], ['!','L','l','H','h'], ['*','/'], ['+','-'], ['>','>=','<=','<','='], ['c', 's', '#']],
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


is_acceptable_left('c', _) -> false;
is_acceptable_left('s', _) -> false;
is_acceptable_left(_, L) -> is_tuple(L) orelse is_number(L) orelse is_list(L) orelse L == '%'.
is_acceptable_right(_, R, _) -> is_tuple(R) orelse is_number(R) orelse is_list(R) orelse R == '%'.

default_left('c') -> '$'; % dummy, it has no left arg
default_left('s') -> '$'; % ditto
default_left('d') -> 1;
default_left(_) -> error.

default_right('d') -> 6;
default_right('!') -> '$';
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

comparison(_, _, AS, BS, AV, BV, OS, OV) when not is_list(AV) andalso not is_list(BV) -> % normal comparison
	Result = OV(AV, BV),
	Color = case Result of
		true -> "3";
		false -> "5"
	end,
	{[AS,3,Color,2,OS,3,2,BS], Result};
comparison(AX, __, AS, BS, AV, BV, OS, OV) when is_list(AV) ->
	Result = lists:map(fun(A) -> {A,OV(A, BV)} end, AV),
	Color = lists:map(fun({_,true}) -> "3"; ({_,false}) -> "5" end, Result),
	case is_literal_list(AX) of
		true ->
			{[$[,32,lists:flatmap(fun({{N,_},C}) -> [3,C,2,integer_to_list(N),3,2,32] end, lists:zip(Result, Color)),$],OS,BS], lists:map(fun({_,A}) -> A end, Result)};
		false ->
			{[$[,32,AS,32,$=,32,lists:flatmap(fun({{N,_},C}) -> [3,C,2,integer_to_list(N),3,2,32] end, lists:zip(Result, Color)),$],OS,BS], lists:map(fun({_,A}) -> A end, Result)}
	end;

comparison(__, BX, AS, BS, AV, BV, OS, OV) when is_list(BV) ->
	Result = lists:map(fun(B) -> {B,OV(AV, B)} end, BV),
	Color = lists:map(fun({_,true}) -> "3"; ({_,false}) -> "5" end, Result),
	case is_literal_list(BX) of
		true ->
			{[AS,OS,$[,32,lists:flatmap(fun({{N,_},C}) -> [3,C,2,integer_to_list(N),3,2,32] end, lists:zip(Result, Color)),$]], lists:map(fun({_,B}) -> B end, Result)};
		false ->
			{[AS,OS,$[,32,BS,32,$=,32,lists:flatmap(fun({{N,_},C}) -> [3,C,2,integer_to_list(N),3,2,32] end, lists:zip(Result, Color)),$]], lists:map(fun({_,B}) -> B end, Result)}
	end.

exploding(N, M, Trg, _) when Trg*20 < M*N -> [0];
exploding(0, _, ___, D) -> D;
exploding(N, M, Trg, D) ->
	{_, NewDice} = rollraw(N, M),
	S = util:count(fun(T) -> T >= Trg end, NewDice),
	exploding(S, M, Trg, NewDice ++ D).

evaluate(Tree, Expand) ->
	case Tree of
		{'#', A, B} ->
			{AS, AV} = evaluate(A, Expand),
			case AV of
				T when is_number(T) andalso T >= 0 andalso T =< 10 ->
					{Strings, Totals} = lists:unzip(lists:map(fun(_) -> evaluate(B, Expand) end, lists:duplicate(T, x))),
					{[AS,$#,$[, string:join(Strings, "; "), $]], Totals};
				_ -> {"{invalid}", 0}
			end;
		{'!', {'d',A,B}, X} ->
			{AS,AV} = evaluate(A, Expand),
			{BS,BV} = evaluate(B, Expand),
			{XS,XV} = evaluate(X, Expand),
			if
				not is_number(AV) orelse
				not is_number(BV) orelse
				not ((is_number(XV) andalso XV<BV andalso XV>1) orelse XV == '$') -> {"{invalid}", 0};
				true ->
					Trg = if XV == '$' -> BV; true -> XV end,
					Dice = exploding(AV, BV, Trg, []),
					Lst = lists:map(fun
							(0) -> [3,$0,$5,2,$e,$r,$r,3,2];
							(T) when T >= Trg -> [3,$0,$9,integer_to_list(T),3,2,2];
							(T) -> integer_to_list(T)
						end, Dice),
					{[$[,AS,$d,BS,$!,XS,$=,2,$[,string:join(Lst, ", "),$],2,$]], lists:sum(Dice)}
			end;

		{Op, X, Y} when Op=='L' orelse Op=='l' orelse Op=='H' orelse Op=='h' ->
			{XS,XV} = evaluate(X, Expand),
			{YS,YV} = evaluate(Y, Expand),
			if
				not (is_list(XV) andalso is_number(YV)) orelse length(XV) < YV -> {"{invalid}", 0};
				true ->
					UseDice = case Op of
						'L' -> lists:sublist(lists:sort(XV), YV); % keep lowest
						'l' -> lists:sublist(lists:sort(XV), YV+1, length(XV)); % drop lowest
						'H' -> lists:sublist(lists:sort(XV), 1+length(XV)-YV, length(XV)); % keep highest
						'h' -> lists:sublist(lists:sort(XV), 1, length(XV)-YV) % drop highest
					end,
					Display = if
						Expand andalso length(UseDice) =< 10 ->
							[$[, string:join(lists:map(fun(T) -> [3,$0,$8,integer_to_list(T),3,2,2] end, UseDice), ", "), $]];
						true -> integer_to_list(lists:foldl(fun erlang:'+'/2, 0, UseDice))
					end,
					{[$[,XS,atom_to_list(Op),YS,$=,2,Display,2,$]], UseDice}
			end;

		{Op, A, B} ->
			{AS,AV} = evaluate(A, Expand),
			{BS,BV} = evaluate(B, Expand),
			case Op of
				's' -> {[$s,$(,BS,$)], sum_or_get(BV)};
				'c' -> {[$c,$(,BS,$)], lists:sum(lists:map(fun(true)->1; (false)->0 end, BV))};
				'+' -> {[AS, $+, BS], sum_or_get(AV) + sum_or_get(BV)};
				'-' -> {[AS, $-, BS], sum_or_get(AV) - sum_or_get(BV)};
				'*' -> {[AS, $*, BS], sum_or_get(AV) * sum_or_get(BV)};
				'/' -> {[AS, $/, BS], sum_or_get(AV) / sum_or_get(BV)};

				'<'  when (not is_list(AV)) orelse  (not is_list(BV)) -> comparison(A, B, AS, BS, AV, BV, "<",  fun erlang:'<' /2);
				'<=' when (not is_list(AV)) orelse  (not is_list(BV)) -> comparison(A, B, AS, BS, AV, BV, "<=", fun erlang:'=<'/2);
				'>'  when (not is_list(AV)) orelse  (not is_list(BV)) -> comparison(A, B, AS, BS, AV, BV, ">",  fun erlang:'>' /2);
				'>=' when (not is_list(AV)) orelse  (not is_list(BV)) -> comparison(A, B, AS, BS, AV, BV, ">=", fun erlang:'>='/2);
				'='  when (not is_list(AV)) orelse  (not is_list(BV)) -> comparison(A, B, AS, BS, AV, BV, "=",  fun erlang:'=='/2);

				'<'  -> comparison(A, B, AS, BS, lists:sum(AV), BV, "<",  fun erlang:'<' /2);
				'<=' -> comparison(A, B, AS, BS, lists:sum(AV), BV, "<=", fun erlang:'=<'/2);
				'>'  -> comparison(A, B, AS, BS, lists:sum(AV), BV, ">",  fun erlang:'>' /2);
				'>=' -> comparison(A, B, AS, BS, lists:sum(AV), BV, ">=", fun erlang:'>='/2);
				'='  -> comparison(A, B, AS, BS, lists:sum(AV), BV, "=",  fun erlang:'=='/2);
				'd'  when is_integer(AV) andalso (is_list(BV) orelse is_integer(BV))->
					case is_list(BV) of
						true ->
							{Stat, RDice, _} = roll(AV,length(BV)),
							Dice = lists:map(fun(T) -> lists:nth(T, BV) end, RDice),
							Total = lists:sum(Dice);
						false ->
							{Stat, Dice, Total} = roll(AV, BV)
					end,
					Display = if
						Expand andalso AV =< 10 -> io_lib:format("~w=~b", [Dice,Total]);
						true -> integer_to_list(Total)
					end,
					Str = case Stat of
						ok -> [$[,AS,$d,BS,$=,2,Display,2,$]];
						X -> [$[,X,AS,$d,BS,$=,2,Display,2,$]]
					end,
					{Str, Dice};
				'i'  when is_integer(AV) andalso is_integer(BV)->
					{Stat, Dice, RTotal} = roll(AV, BV),
					Avg = AV * ((BV-1)/2 + 1),
					case round(Avg) == Avg of
						true -> {"{invalid}", 0};
						false ->
							Adj = (AV*BV - AV + 1) div 2,
							case RTotal > Avg of
								true -> Total = RTotal - Adj;
								false -> Total = RTotal + Adj
							end,
							Display = if
								Expand andalso AV =< 10 -> io_lib:format("~w=~p", [Dice,Total]);
								true -> io_lib:format("~p", [Total])
							end,
							Str = case Stat of
								ok -> [$[,AS,$i,BS,$=,2,Display,2,$]];
								X -> [$[,X,AS,$i,BS,$=,2,Display,2,$]]
							end,
							{Str, Total}
					end;
				_ -> {"{invalid}", 0}
			end;
		{list, L} ->
			{Strs, Vals} = lists:unzip(lists:map(fun(T) -> evaluate(T, Expand) end, L)),
			{[$[,string:join(Strs, ", "),$]], lists:flatten(Vals)};
		[XTree] ->
			{S,V} = evaluate(XTree, Expand),
			{[$(,S,$)],V};
		T when is_list(T) -> {io_lib:format("~p", [T]), T};
		'$' -> {"", '$'};
		'%' -> {"%", 100};
		T -> {integer_to_list(T), T}
	end.

sum_or_get(L) when is_list(L) -> lists:sum(L);
sum_or_get(L) -> L.

is_literal_list([_]) -> false;
is_literal_list(T) when is_list(T) -> true;
is_literal_list(_) -> false.

roll(N,_) when N < 0 -> {"{invalid} ", [], 0};
roll(_,M) when M =< 0 -> {"{invalid} ", [], 0};
roll(N,_) when N > 1000000 -> {"\x035\x02X\x03\x02 ", [], 0};
roll(_,M) when M > 1000000 -> {"\x035\x02X\x03\x02 ", [], 0};
roll(N,M) ->
	case rollraw(N,M) of
		{Stat, Dice} when is_list(Dice) -> {Stat, Dice, lists:foldl(fun erlang:'+'/2, 0, Dice)};
		{Stat, Total} -> {Stat, [], Total}
	end.

rollraw(N, 0) -> {ok, lists:duplicate(N, 0)};
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
