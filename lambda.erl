-module(lambda).
-export([y/1]).

y(X) ->
	F = fun (P) -> X(fun (Arg) -> (P(P))(Arg) end) end,
	F(F).
