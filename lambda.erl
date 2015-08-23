-module(lambda).
-compile(export_all).

y(X) ->
	F = fun (P) -> X(fun (Arg) -> (P(P))(Arg) end) end,
	F(F).

ekey(K, V, NV) ->
	K2 = K - ((NV bsl 16) + V),
	if
		K2 < 0 -> io:format("~b~n",[K2]), K2 + 16#100000000;
		true -> K2
	end.

dkey(K2, V, NV) ->
	K = K2 + ((NV bsl 16) + V),
	if
		K < 0 -> io:format("~b~n",[K]), K + 16#100000000;
		true -> K
	end.

decode(D,K) ->
	{ND,C} = lists:mapfoldl(fun(X,A) ->
			NX = (X - ((K bsr (A band 16#1f)) + A)) band 16#ff,
	%		io:fwrite("X=~2.16b, A=~2.16b, NX=~2.16b, NA=~2.16b~n", [X, A, NX, (A+NX)band 16#ff]),
			{NX, (A+NX) band 16#ff}
		end, 0, lists:reverse(tl(lists:reverse(D)))),
	case lists:last(D) of
		C -> {ok, ND};
		_ -> {C, ND}
	end.

encode(D,K) ->
	{ND,C} = lists:mapfoldl(fun(X,A) ->
			NX = (X + ((K bsr (A band 16#1f)) + A)) band 16#ff,
			{NX, (A+X) band 16#ff}
		end, 0, D),
	ND ++ [C].
