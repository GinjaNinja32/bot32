-module(z_dice2).
-compile(export_all).

get_commands() ->
	[
		{"dice", fun dice/5, user},
		{"roll", fun dice/5, user},
		{"rtd",  fun dice/5, user},
		{"edice",  fun edice/5, user},
		{"eroll",  fun edice/5, user},
		{"ertd",   fun edice/5, user},
		{"exdice", fun edice/5, user},
		{"exroll", fun edice/5, user},
		{"exrtd",  fun edice/5, user}
	].

initialise(T)->T.
deinitialise(T)->T.

dice(_, RT, P, Params, _) -> {irc, {msg, {RT, [P, dice(string:join(Params, []), false)]}}}.
edice(_, RT, P, Params, _) -> {irc, {msg, {RT, [P, dice(string:join(Params, []), true)]}}}.

get_real_tokens({A,B,_,[N,'<' |T]}) -> get_real_tokens({A,B,  N,  T});
get_real_tokens({A,B,_,[N,'<='|T]}) -> get_real_tokens({A,B,  N+1,T});
get_real_tokens({A,_,_,[N,'=' |T]}) -> get_real_tokens({A,N-1,N+1,T});
get_real_tokens({A,_,C,[N,'>' |T]}) -> get_real_tokens({A,N,  C,  T});
get_real_tokens({A,_,C,[N,'>='|T]}) -> get_real_tokens({A,N-1,C,  T});
get_real_tokens({_,B,C,[N,'#' |T]}) -> get_real_tokens({N,B,  C,  T});
get_real_tokens({A,B,C,X}) -> get_real_tokens2({A,B,C,lists:reverse(X)}).

get_real_tokens2({A,B,_,[N,'<' |T]}) -> get_real_tokens2({A,B,  N,  T});
get_real_tokens2({A,B,_,[N,'<='|T]}) -> get_real_tokens2({A,B,  N+1,T});
get_real_tokens2({A,_,_,[N,'=' |T]}) -> get_real_tokens2({A,N-1,N+1,T});
get_real_tokens2({A,_,C,[N,'>' |T]}) -> get_real_tokens2({A,N,  C,  T});
get_real_tokens2({A,_,C,[N,'>='|T]}) -> get_real_tokens2({A,N-1,C,  T});
get_real_tokens2({_,B,C,[N,'#' |T]}) -> get_real_tokens2({N,B,  C,  T});
get_real_tokens2({A,B,C,X}) -> {A,B,C,lists:reverse(X)}.

dice(String, Expand) ->
	case tokenise(String) of
		error -> "Error parsing dice string!";
		Tokens ->
			{N,Min,Max,Toks} = get_real_tokens({1,no,no,Tokens}),
			Expressions = parse(Toks),
			if
				N =< 0 -> "You want me to roll what?";
				N == 1 -> evaluateMany(Expressions, Expand, Min, Max);
				N =< 20 ->
					Rolls = lists:map(fun(_) -> evaluateMany(Expressions, Expand, Min, Max) end, lists:duplicate(N, x)),
					[$[, string:join(Rolls, ", "),32,$]];
				true -> "Too many sets, try less."
			end
	end.

% TOKENISATION

tokenise(String) ->
	case tokenise(String, []) of
		error -> error;
		T -> lists:reverse(T)
	end.

tokenise([], T) -> T;

tokenise([$ |S],T) -> tokenise(S, T);
tokenise([$	|S],T) -> tokenise(S, T);

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
tokenise([$D|S],T) -> tokenise(S, ['d'|T]);

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
	lists:reverse(parse_add(
		lists:reverse(parse_mult(
			lists:reverse(parse_d(Tokens, [])),
		[])),
	[])).

parse_d([         ], X) -> X;
parse_d([N,'d',M|S], X) -> parse_d(S, [{'d',N,M}|X]);
parse_d([T      |S], X) -> parse_d(S, [T        |X]).

parse_mult([         ], X) -> X;
parse_mult([N,'*',M|S], X) -> parse_mult(S, [{'*',   N, M}|   X ]);
parse_mult(['*',N  |S], X) -> parse_mult(S, [{'*',hd(X),N}|tl(X)]);
parse_mult([T      |S], X) -> parse_mult(S, [          T  |   X ]).

parse_add([         ], X) -> X;
parse_add([N,'+',M|S], X) -> parse_add(S, [{'+',   N, M}|   X ]);
parse_add(['+',N  |S], X) -> parse_add(S, [{'+',hd(X),N}|tl(X)]);
parse_add([N,'-',M|S], X) -> parse_add(S, [{'-',   N, M}|   X ]);
parse_add(['-',N  |S], X) -> parse_add(S, [{'-',hd(X),N}|tl(X)]);
parse_add([T      |S], X) -> parse_add(S, [          T  |   X ]).

% EVALUATION

evaluateMany(Expressions, Expand, Min, Max) ->
	string:join(lists:map(fun(Expr) -> evaluate(Expr, Expand, Min, Max) end, Expressions), "; ").

evaluate(T, Expand, Min, Max) ->
	{X,Y} = eval(T, Expand),
	C = if
		Min == no andalso Max == no -> $8;
		Min == no andalso X < Max -> $9;
		Max == no andalso X > Min -> $9;
		Min < X andalso X < Max -> $9;
		true -> $4
	end,
	[3,C,32,integer_to_list(X), "\x0315 : ", Y, "\x03"].

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
eval(E, _) -> {E, integer_to_list(E)}.

% ROLLING

roll(0, _, _) -> {0, "0"};
roll(_, 0, _) -> {0, "0"};
roll(X, _, _) when X > 1000000 -> {0, "X"};
roll(_, X, _) when X > 1000000 -> {0, "X"};
roll(N, M, false) ->
	R = lists:foldl(fun(_,X) ->
			X + random:uniform(M)
		end, 0, lists:duplicate(N, x)),
	{R, integer_to_list(R)};
roll(N, M, true) ->
	{R,P} = lists:foldl(fun(_,{X,E}) ->
			A = random:uniform(M),
			{X + A, [A|E]}
		end, {0,[]}, lists:duplicate(N, x)),
	{R, io_lib:format("~w", [P])}.
