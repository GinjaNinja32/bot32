-module(dice2).
-compile(export_all).

-include("colordefs.hrl").

get_commands() ->
	lists:map(fun({K,F}) ->
			{K, fun(#{reply:=RT, ping:=P, params:=Params}) -> {irc, {msg, {RT, [P, F(Params)]}}} end, user}
		end, special_dice())
	++ [
		{"dice2", fun dice/1, user},
		{"edice2",  fun edice/1, user}
	].

get_help("dice2") ->
	[
		"'dice XdY' for a basic dice roll, using +/-/* to modify the value.",
		"Basic comparisons can be done using >, >=, <, <=, and =.",
		"Prepend a 'c' or 'f' to the previous to set critical-success or critical-failure bounds instead.",
		"Add an 's' at either end to show a summary rather than all rolls.",
		"Use 'edice' for an exploded roll, i.e. \"14 : [4,5,5]\" instead of \"14 : 14\"."
	];
get_help("edice2") -> get_help("dice2");
get_help(_) -> unhandled.

special_dice() ->
	[
		{"gurps2", fun([]) -> dice("3d6", true);
		             ([StrT | _]) ->
				T = list_to_integer(StrT),
				{S,F,B} = if
					T >= 16 -> {6, 18, 16};
					T == 15 -> {5, 17, 15};
					T =< 6 -> {4, T+10, T};
					true -> {4, 17, T}
				end,
				dice(lists:flatten(io_lib:format("~bc>= ~bf<= 3d6<=~b", [S, F, B])), true)
			end},
		{"srun2", fun([StrT | _]) ->
				T = list_to_integer(StrT),
				{_, Dice} = rollraw(T, 6),
				{One,FivePlus} = lists:foldl(fun(1, {O,F}) -> {O+1,F}; (N, {O,F}) when N >= 5 -> {O, F+1}; (_, D) -> D end, {0,0}, Dice),
				Summary = io_lib:format("~b failure~s, ~b hit~s", [One, util:s(One), FivePlus, util:s(FivePlus)]),
				if
					2*One >= T andalso FivePlus == 0 -> [Summary | ": Critical Glitch!"];
					2*One >= T -> [Summary | ": Glitch!"];
					true -> Summary
				end
			end},
		{"sredge2", fun([StrT | _]) ->
				T = list_to_integer(StrT),
				{ExtraDice, Dice} = get_sr_edge(T, 0, []),
				{One,FivePlus} = lists:foldl(fun(1, {O,F}) -> {O+1,F}; (N, {O,F}) when N >= 5 -> {O, F+1}; (_, D) -> D end, {0,0}, Dice),
				Summary = io_lib:format("~b failure~s, ~b hit~s (rolled ~b six~s)", [One, util:s(One), FivePlus, util:s(FivePlus), ExtraDice, util:s(ExtraDice, "es")]),
				if
					2*One >= T andalso FivePlus == 0 -> [Summary | ": Critical Glitch!"];
					2*One >= T -> [Summary | ": Glitch!"];
					true -> Summary
				end
			end},
		{"fate2", fun([]) -> fate(0);
		            ([StrT | _]) -> fate(list_to_integer(StrT))
			end}
	].

fate(N) ->
	{_, RawDice} = rollraw(4, 3),
	Dice = lists:map(fun(T) -> T-2 end, RawDice), % d3-2 is equivalent to a single FATE die
	Str = lists:map(fun(-1) -> ?LRED ++ "-" ++ ?RESET;
	                   ( 0) -> ?WHITE ++ ?BOLD ++ ?BOLD ++ "0" ++ ?RESET;
	                   (+1) -> ?LGREEN ++ "+" ++ ?RESET
		end, Dice),
	Total = lists:foldl(fun erlang:'+'/2, 0, Dice),
	io_lib:format("[~s|~s] ~s", [Str, show(Total), show(Total+N)]).

show(N) when N < 0 -> io_lib:format("~s~b~s", [?LRED, N, ?RESET]);
show(N) when N > 0 -> io_lib:format("~s+~b~s", [?LGREEN, N, ?RESET]);
show(0) -> ?WHITE ++ "+0" ++ ?RESET.

get_sr_edge(0, Sixes, D) -> {Sixes, D};
get_sr_edge(N, Sixes, D) ->
	{_, NewDice} = rollraw(N, 6),
	S = util:count(fun(T) -> T == 6 end, NewDice),
	get_sr_edge(S, S+Sixes, NewDice ++ D).

get_dicemode() ->
	case config:get_value(config, [?MODULE, dicemode], undefined) of
		internal -> "internal RNG";
		random -> "random.org";
		undefined -> "undefined (using internal)";
		_ -> "unknown"
	end.

dicemode(#{reply:=RT, ping:=P, params:=[]}) -> {irc, {msg, {RT, [P, "Dicemode is ", get_dicemode()]}}};
dicemode(#{reply:=RT, ping:=P, params:=[Mode]}) ->
	Reply = case Mode of
		"random" -> config:set_value(config, [?MODULE, dicemode], random), "Dicemode set to random.org.";
		"internal" -> config:set_value(config, [?MODULE, dicemode], internal), "Dicemode set to internal RNG";
		_ -> ["Unknown dicemode '", Mode, $']
	end,
	{irc, {msg, {RT, [P, Reply]}}}.


dice (#{reply:=RT, ping:=P, params:=[]    }) -> {irc, {msg, {RT, [P, "Provide some dice to roll, or try 'help dice' for help."]}}};
dice (#{reply:=RT, ping:=P, params:=Params}) -> {irc, {msg, {RT, [P, dice(string:join(Params, " "), false)]}}}.
edice(#{reply:=RT, ping:=P, params:=[]    }) -> {irc, {msg, {RT, [P, "Provide some dice to roll, or try 'help dice' for help."]}}};
edice(#{reply:=RT, ping:=P, params:=Params}) -> {irc, {msg, {RT, [P, dice(string:join(Params, " "), true)]}}}.

-record(dp, {n=1, min=no, max=no, csl=no, csm=no, cfl=no, cfm=no, show=false}).

get_real_tokens(D,[    N,'>' |T]) -> get_real_tokens(D#dp{max= N            }, T);
get_real_tokens(D,[    N,'>='|T]) -> get_real_tokens(D#dp{max= N+1          }, T);
get_real_tokens(D,[    N,'=' |T]) -> get_real_tokens(D#dp{max= N+1, min= N-1}, T);
get_real_tokens(D,[    N,'<='|T]) -> get_real_tokens(D#dp{          min= N-1}, T);
get_real_tokens(D,[    N,'<' |T]) -> get_real_tokens(D#dp{          min= N  }, T);
get_real_tokens(D,['-',N,'>' |T]) -> get_real_tokens(D#dp{max=-N            }, T);
get_real_tokens(D,['-',N,'>='|T]) -> get_real_tokens(D#dp{max=-N-1          }, T);
get_real_tokens(D,['-',N,'=' |T]) -> get_real_tokens(D#dp{max=-N-1, min=-N+1}, T);
get_real_tokens(D,['-',N,'<='|T]) -> get_real_tokens(D#dp{          min=-N+1}, T);
get_real_tokens(D,['-',N,'<' |T]) -> get_real_tokens(D#dp{          min=-N  }, T);

get_real_tokens(D,[    N,'c>' |T]) -> get_real_tokens(D#dp{csl= N            }, T);
get_real_tokens(D,[    N,'c>='|T]) -> get_real_tokens(D#dp{csl= N+1          }, T);
get_real_tokens(D,[    N,'c=' |T]) -> get_real_tokens(D#dp{csl= N+1, csm= N-1}, T);
get_real_tokens(D,[    N,'c<='|T]) -> get_real_tokens(D#dp{          csm= N-1}, T);
get_real_tokens(D,[    N,'c<' |T]) -> get_real_tokens(D#dp{          csm= N  }, T);
get_real_tokens(D,['-',N,'c>' |T]) -> get_real_tokens(D#dp{csl=-N            }, T);
get_real_tokens(D,['-',N,'c>='|T]) -> get_real_tokens(D#dp{csl=-N-1          }, T);
get_real_tokens(D,['-',N,'c=' |T]) -> get_real_tokens(D#dp{csl=-N-1, csm=-N+1}, T);
get_real_tokens(D,['-',N,'c<='|T]) -> get_real_tokens(D#dp{          csm=-N+1}, T);
get_real_tokens(D,['-',N,'c<' |T]) -> get_real_tokens(D#dp{          csm=-N  }, T);

get_real_tokens(D,[    N,'f>' |T]) -> get_real_tokens(D#dp{cfl= N            }, T);
get_real_tokens(D,[    N,'f>='|T]) -> get_real_tokens(D#dp{cfl= N+1          }, T);
get_real_tokens(D,[    N,'f=' |T]) -> get_real_tokens(D#dp{cfl= N+1, cfm= N-1}, T);
get_real_tokens(D,[    N,'f<='|T]) -> get_real_tokens(D#dp{          cfm= N-1}, T);
get_real_tokens(D,[    N,'f<' |T]) -> get_real_tokens(D#dp{          cfm= N  }, T);
get_real_tokens(D,['-',N,'f>' |T]) -> get_real_tokens(D#dp{cfl=-N            }, T);
get_real_tokens(D,['-',N,'f>='|T]) -> get_real_tokens(D#dp{cfl=-N-1          }, T);
get_real_tokens(D,['-',N,'f=' |T]) -> get_real_tokens(D#dp{cfl=-N-1, cfm=-N+1}, T);
get_real_tokens(D,['-',N,'f<='|T]) -> get_real_tokens(D#dp{          cfm=-N+1}, T);
get_real_tokens(D,['-',N,'f<' |T]) -> get_real_tokens(D#dp{          cfm=-N  }, T);

get_real_tokens(D,[    N,'#' |T]) -> get_real_tokens(D#dp{n=N}, T);
get_real_tokens(D,[      's' |T]) -> get_real_tokens(D#dp{show=true}, T);

get_real_tokens(D,X) -> get_real_tokens2(D, lists:reverse(X)).

get_real_tokens2(D,[N,    '<' |T]) -> get_real_tokens2(D#dp{max= N            }, T);
get_real_tokens2(D,[N,    '<='|T]) -> get_real_tokens2(D#dp{max= N+1          }, T);
get_real_tokens2(D,[N,    '=' |T]) -> get_real_tokens2(D#dp{max= N+1, min= N-1}, T);
get_real_tokens2(D,[N,    '>='|T]) -> get_real_tokens2(D#dp{          min= N-1}, T);
get_real_tokens2(D,[N,    '>' |T]) -> get_real_tokens2(D#dp{          min= N  }, T);
get_real_tokens2(D,[N,'-','<' |T]) -> get_real_tokens2(D#dp{max=-N            }, T);
get_real_tokens2(D,[N,'-','<='|T]) -> get_real_tokens2(D#dp{max=-N-1          }, T);
get_real_tokens2(D,[N,'-','=' |T]) -> get_real_tokens2(D#dp{max=-N-1, min=-N+1}, T);
get_real_tokens2(D,[N,'-','>='|T]) -> get_real_tokens2(D#dp{          min=-N+1}, T);
get_real_tokens2(D,[N,'-','>' |T]) -> get_real_tokens2(D#dp{          min=-N  }, T);

get_real_tokens2(D,[N,    'c<' |T]) -> get_real_tokens2(D#dp{csl= N            }, T);
get_real_tokens2(D,[N,    'c<='|T]) -> get_real_tokens2(D#dp{csl= N+1          }, T);
get_real_tokens2(D,[N,    'c=' |T]) -> get_real_tokens2(D#dp{csl= N+1, csm= N-1}, T);
get_real_tokens2(D,[N,    'c>='|T]) -> get_real_tokens2(D#dp{          csm= N-1}, T);
get_real_tokens2(D,[N,    'c>' |T]) -> get_real_tokens2(D#dp{          csm= N  }, T);
get_real_tokens2(D,[N,'-','c<' |T]) -> get_real_tokens2(D#dp{csl=-N            }, T);
get_real_tokens2(D,[N,'-','c<='|T]) -> get_real_tokens2(D#dp{csl=-N-1          }, T);
get_real_tokens2(D,[N,'-','c=' |T]) -> get_real_tokens2(D#dp{csl=-N-1, csm=-N+1}, T);
get_real_tokens2(D,[N,'-','c>='|T]) -> get_real_tokens2(D#dp{          csm=-N+1}, T);
get_real_tokens2(D,[N,'-','c>' |T]) -> get_real_tokens2(D#dp{          csm=-N  }, T);

get_real_tokens2(D,[N,    'f<' |T]) -> get_real_tokens2(D#dp{cfl= N            }, T);
get_real_tokens2(D,[N,    'f<='|T]) -> get_real_tokens2(D#dp{cfl= N+1          }, T);
get_real_tokens2(D,[N,    'f=' |T]) -> get_real_tokens2(D#dp{cfl= N+1, cfm= N-1}, T);
get_real_tokens2(D,[N,    'f>='|T]) -> get_real_tokens2(D#dp{          cfm= N-1}, T);
get_real_tokens2(D,[N,    'f>' |T]) -> get_real_tokens2(D#dp{          cfm= N  }, T);
get_real_tokens2(D,[N,'-','f<' |T]) -> get_real_tokens2(D#dp{cfl=-N            }, T);
get_real_tokens2(D,[N,'-','f<='|T]) -> get_real_tokens2(D#dp{cfl=-N-1          }, T);
get_real_tokens2(D,[N,'-','f=' |T]) -> get_real_tokens2(D#dp{cfl=-N-1, cfm=-N+1}, T);
get_real_tokens2(D,[N,'-','f>='|T]) -> get_real_tokens2(D#dp{          cfm=-N+1}, T);
get_real_tokens2(D,[N,'-','f>' |T]) -> get_real_tokens2(D#dp{          cfm=-N  }, T);

get_real_tokens2(D,[N,    '#' |T]) -> get_real_tokens2(D#dp{n=N}, T);
get_real_tokens2(D,[      's' |T]) -> get_real_tokens2(D#dp{show=true}, T);
get_real_tokens2(D,X) -> {D, lists:reverse(X)}.

dice(String, Expand) ->
%	common:debug("dice", "~p | ~p", [String, Expand]),
	case tokenise(String) of
		error -> "Error parsing dice string!";
		Tokens ->
%			common:debug("dice", "~p", [Tokens]),
			{DP, Toks} = get_real_tokens(#dp{}, Tokens),
			case parse(Toks) of
				error -> "Error parsing dice string!";
				Expressions ->
					if
						DP#dp.n =< 0 -> "You want me to roll what?";
						DP#dp.n == 1 ->
							{_,A} = evaluateMany(Expressions, Expand, DP),
							A;
						DP#dp.n =< 20 ->
							Rolls = lists:map(fun(_) -> evaluateMany(Expressions, Expand, DP) end, lists:duplicate(DP#dp.n, x)),
							if
								DP#dp.show ->
									What = lists:map(fun({T,_}) -> T end, Rolls),
									X = lists:foldl(fun(A, B) ->
											orddict:fold(fun(K, V, Ac) ->
													case orddict:find(K, Ac) of
														{ok, Val} -> orddict:store(K, Val+V, Ac);
														error -> orddict:store(K, V, Ac)
													end
												end, A, B)
										end, hd(What), tl(What)),
									string:join(lists:map(fun({A,B}) -> io_lib:format("~s: ~b", [describe(A),B]) end, X), "; ");
								true ->
									[$[, string:join(lists:map(fun({_,T})->T end, Rolls), ", "),32,$]]
							end;
						true -> "Too many sets, try less."
					end
			end
	end.

% TOKENISATION

tokenise(String) ->
	case tokenise(string:to_lower(String), []) of
		error -> error;
		T -> lists:reverse(T)
	end.

tokenise([], T) -> T;

tokenise([$ |S],T) -> tokenise(S, T);
tokenise([$	|S],T) -> tokenise(S, T);

tokenise([$c,$>,$=|S],T) -> tokenise(S, ['c>='|T]);
tokenise([$c,$<,$=|S],T) -> tokenise(S, ['c<='|T]);
tokenise([$c,$=,$<|S],T) -> tokenise(S, ['c<='|T]);
tokenise([$c,$=|S],T) -> tokenise(S, ['c='|T]);
tokenise([$c,$>|S],T) -> tokenise(S, ['c>'|T]);
tokenise([$c,$<|S],T) -> tokenise(S, ['c<'|T]);

tokenise([$f,$>,$=|S],T) -> tokenise(S, ['f>='|T]);
tokenise([$f,$<,$=|S],T) -> tokenise(S, ['f<='|T]);
tokenise([$f,$=,$<|S],T) -> tokenise(S, ['f<='|T]);
tokenise([$f,$=|S],T) -> tokenise(S, ['f='|T]);
tokenise([$f,$>|S],T) -> tokenise(S, ['f>'|T]);
tokenise([$f,$<|S],T) -> tokenise(S, ['f<'|T]);

tokenise([$>,$=|S],T) -> tokenise(S, ['>='|T]);
tokenise([$<,$=|S],T) -> tokenise(S, ['<='|T]);
tokenise([$=,$<|S],T) -> tokenise(S, ['<='|T]);
tokenise([$=|S],T) -> tokenise(S, ['='|T]);
tokenise([$>|S],T) -> tokenise(S, ['>'|T]);
tokenise([$<|S],T) -> tokenise(S, ['<'|T]);
tokenise([$#|S],T) -> tokenise(S, ['#'|T]);
tokenise([$+|S],T) -> tokenise(S, ['+'|T]);
tokenise([$-|S],T) -> tokenise(S, ['-'|T]);
tokenise([$*|S],T) -> tokenise(S, ['*'|T]);
tokenise([$d|S],T) -> tokenise(S, ['d'|T]);
tokenise([$(|S],T) -> tokenise(S, ['('|T]);
tokenise([$)|S],T) -> tokenise(S, [')'|T]);
tokenise([$s|S],T) -> tokenise(S, ['s'|T]);

tokenise([X|S], T) ->
	case lists:member(X, lists:seq($0, $9)) of
		true ->
			{A,B} = tokenise_num([X|S], T),
			tokenise(A, B);
		false ->
			case lists:member(X, [$;, $:, $|]) of
				true -> T;
				false -> error
			end
	end.

tokenise_num(S,T) -> tokenise_num(S,T,0).

tokenise_num([],T,X)->{[], [X|T]};
tokenise_num([D|S],T,X) when $0 =< D andalso D =< $9 -> tokenise_num(S,T,10*X + (D-$0));
tokenise_num(S,T,X) -> {S, [X|T]}.

% PARSING

parse(Tokens) ->
	List = lists:reverse(parse_add(
		lists:reverse(parse_mult(
			lists:reverse(parse_d(Tokens, [])),
		[])),
	[])),
	logging:log(debug, "DICE2", "parse pass A: ~p", [List]),
	case lists:member('(', List) andalso lists:member(')', List) of
		true -> case collapse_brackets(List, []) of
				error -> error;
				L ->
					case lists:reverse(L) of
						Tokens -> error;
						L2 ->
							logging:log(debug, "DICE2", "parse pass B: ~p", [L2]),
							parse(L2)
					end
			end;
		false -> List
	end.

parse_d([         ], X) -> X;
parse_d([N,'d',M|S], X) when is_integer(N) andalso is_integer(M)-> parse_d(S, [{'d',N,M}|X]);
parse_d([N,'d'  |S], X) when is_integer(N) -> parse_d(S, [{'d',N,6}|X]);
parse_d([  'd',M|S], X) when is_integer(M) -> parse_d(S, [{'d',1,M}|X]);
parse_d([  'd'  |S], X) -> parse_d(S, [{'d',1,6}|X]);
parse_d([T      |S], X) -> parse_d(S, [T        |X]).

parse_mult([         ], X) -> X;
parse_mult([N,'*',M|S], X) when not is_atom(N) andalso not is_atom(   M ) -> parse_mult(S, [{'*',   N, M}|   X ]);
parse_mult(['*',N  |S], X) when not is_atom(N) andalso not is_atom(hd(X)) -> parse_mult(S, [{'*',hd(X),N}|tl(X)]);
parse_mult([T      |S], X) -> parse_mult(S, [          T  |   X ]).

parse_add([         ], X) -> X;
parse_add([N,'+',M|S], X) when not is_atom(N) andalso not is_atom(   M ) -> parse_add(S, [{'+',   N, M}|   X ]);
parse_add(['+',N  |S], X) when not is_atom(N) andalso not is_atom(hd(X)) -> parse_add(S, [{'+',hd(X),N}|tl(X)]);
parse_add([N,'-',M|S], X) when not is_atom(N) andalso not is_atom(   M ) -> parse_add(S, [{'-',   N, M}|   X ]);
parse_add(['-',N  |S], X) when not is_atom(N) andalso not is_atom(hd(X)) -> parse_add(S, [{'-',hd(X),N}|tl(X)]);
parse_add([T      |S], X) -> parse_add(S, [          T  |   X ]).

collapse_brackets(['(',N,')'|S], X) when not is_atom(N) -> collapse_brackets(S, [{N}|X]);
collapse_brackets(['(',N,')'|_], _) when is_atom(N) -> error;
collapse_brackets([    N    |S], X) -> collapse_brackets(S, [N|X]);
collapse_brackets([           ], X) -> X.

% EVALUATION

evaluateMany(Expressions, Expand, DP) ->
	Rolls = lists:map(fun(Expr) -> evaluate(Expr, Expand, DP) end, Expressions),

	Results = lists:foldl(fun({What, _}, Dict) ->
		case orddict:find(What, Dict) of
			{ok, Value} -> orddict:store(What, Value+1, Dict);
			error -> orddict:store(What, 1, Dict)
		end
	end, [], Rolls),
	{Results, string:join(lists:map(fun({_, R}) -> R end, Rolls), " ")}.

evaluate(T, Expand, DP) ->
	{X,Y} = eval(T, Expand),
%	common:debug("rolling", "~p (~p) -> ~p : ~p", [T, Expand, X, Y]),
	C = rolltype(X, DP),
	Color = orddict:fetch(C, [
			{cf, "6"},
			{cs, "11"},
			{f, "5"},
			{n, ""},
			{s, "3"}
		]),
	{C, [2,3,Color,32,integer_to_list(X), "\x02\x0315 : ", Y, "\x03"]}.

-define(bt(Min, N, Max), (Min /= no orelse Max /= no) andalso ((Min == no orelse Min < N) andalso (Max == no orelse N < Max))).

rolltype(Roll, D=#dp{min=Min, max=Max, csl=CSL, csm=CSM, cfl=CFL, cfm=CFM}) ->
%	common:debug("checking rolltype", "~p, ~p", [Roll, D]),
	if
		?bt(CSM, Roll, CSL) -> cs;
		?bt(CFM, Roll, CFL) -> cf;
		?bt(Min, Roll, Max) -> s;
		true -> case lists:all(fun(T) -> T == no end, [D#dp.min, D#dp.max, D#dp.csl, D#dp.csm, D#dp.cfl, D#dp.cfm]) of
				true -> n;
				false -> f
			end
	end.

eval({Op,N,M}, X) ->
	{L,LT} = eval(N, X),
	{R,RT} = eval(M, X),
	case Op of
		'+' -> {L+R, [LT,32,$+,32,RT]};
		'-' -> {L-R, [LT,32,$-,32,RT]};
		'*' -> {L*R, [LT,$*,RT]};
		'd' ->
			{Val,Exp} = roll(L, R, X),
			{Val, [2,Exp,2]}
	end;
eval({E}, X) -> {A,B} = eval(E,X), {A, [$(,B,$)]};
eval(E, _) -> {E, integer_to_list(E)}.

% ROLLING

roll(0, _, _) -> {0, "0"};
roll(_, 0, _) -> {0, "0"};
roll(X, _, _) when X > 1000000 -> {0, "X"};
roll(_, X, _) when X > 1000000 -> {0, "X"};
roll(N, M, Expand) ->
	{S, Rolls} = rollraw(N, M),
	Total = lists:foldl(fun erlang:'+'/2, 0, Rolls),
	Message = case Expand of
		true when N =< 10 -> io_lib:format("~w", [Rolls]);
		true -> ["\x034!\x03 " | integer_to_list(Total)];
		false -> integer_to_list(Total)
	end,
	case S of
		ok -> {Total, Message};
		_ -> {Total, [S | Message]}
	end.

rollraw(N, 1) -> {ok, lists:duplicate(N, 1)};
rollraw(N, M) ->
	case config:get_value(config, [?MODULE, dicemode], internal) of
		random when N > 100 -> {"\x038!\x03 ", lists:map(fun(_) -> random:uniform(M) end, lists:duplicate(N, x))};
		random when M =< 100 -> {ok, get_n_m(N, M)};
		_ -> {ok, lists:map(fun(_) -> random:uniform(M) end, lists:duplicate(N, x))}
	end.

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
	Dict = config:get_value(temp, [?MODULE, cache], []),
	case orddict:find(M, Dict) of
		{ok, List} -> List;
		error -> []
	end.

set_avail(M, List) ->
	Dict = config:get_value(temp, [?MODULE, cache], []),
	config:set_value(temp, [?MODULE, cache], orddict:store(M, List, Dict)).

describe(f) -> "failures";
describe(s) -> "successes";
describe(cf) -> "critical failures";
describe(cs) -> "critical successes";
describe(n) -> "neutral";
describe(_) -> "???".
