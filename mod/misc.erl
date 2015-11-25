-module(misc).
-compile(export_all).

get_commands() ->
	[
		{"hexnc", fun hexnc/1, user}
	].

hexchat() -> ["03", "04", "06", "08", "09", "10", "11", "12", "13"].

hexnc(#{reply:=RT, ping:=P, params:=Params}) ->
	Rep = lists:map(fun(T) ->
			C = lists:foldl(fun erlang:'+'/2, 0, T) rem 9,
			[3, lists:nth(C + 1, hexchat()), T]
		end, Params),
	{irc, {msg, {RT, [P, string:join(Rep, " ")]}}}.
