-module(cmd_echo).
-compile(export_all).

get_commands() ->
	[
		{"echo", fun echo/1, [long], user}
	].

echo(#{reply:=Reply, ping:=Ping, params:=[String]}) ->
	{irc, {msg, {Reply, [Ping, String]}}}.
