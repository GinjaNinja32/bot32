-module(misc).
-compile(export_all).

get_commands() ->
	[
		{"hexnc", fun hexnc/1, user},
		{"irc", fun irc/1, user},
		{"webchat", fun webchat/1, user},
		{"xkcd", fun xkcd/1, user}
	].

hexchat() -> ["03", "04", "06", "08", "09", "10", "11", "12", "13"].

hexnc(#{reply:=RT, ping:=P, params:=Params}) ->
	Rep = lists:map(fun(T) ->
			C = lists:foldl(fun erlang:'+'/2, 0, T) rem 9,
			[3, lists:nth(C + 1, hexchat()), T]
		end, Params),
	{irc, {msg, {RT, [P, string:join(Rep, " ")]}}}.

irc(#{reply:=RT, ping:=P}) ->
	{irc, {msg, {RT, [P, io_lib:format("http://webchat.sorcery.net/?channels=~s or ~s on irc.sorcery.net", [RT, RT])]}}}.

webchat(#{reply:=RT, ping:=P}) ->
	{irc, {msg, {RT, [P, io_lib:format("http://webchat.sorcery.net/?channels=~s", [RT])]}}}.

xkcd(#{reply:=RT, ping:=P, params:=Params}) ->
	os:putenv("xkcd", string:join(Params, " ")),
	Reply = util:safe_os_cmd("./xkcd.sh \"$xkcd\""),

	{irc, {msg, {RT, [P, Reply]}}}.
