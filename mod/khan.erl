-module(khan).
-compile(export_all).

do_extras(Tokens, RC, _) ->
	case lists:filter(fun is_khan/1, Tokens) of
		[] -> ok;
		[Khan|_] ->
			Len = length(Khan),
			core ! {irc, {msg, {RC, [$K, $H, lists:duplicate(Len, $A), $N, lists:duplicate(2 + (Len div 10), $!)]}}}
	end.

is_khan(List) ->
	lists:all(fun(T) -> lists:member(T, List) orelse lists:member(T + 32, List) end, "KHAN")
	andalso lists:all(fun(T) -> lists:member(T, "KHANkhan!?") end, List).
