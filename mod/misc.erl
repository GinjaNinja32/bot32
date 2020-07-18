-module(misc).
-compile(export_all).

-define(BASE_URL, "https://kiwiirc.com/nextclient/irc.sorcery.net:+6697/").

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

irc(#{reply:=RT, ping:=P, params:=Params}) ->
	case Params of
		[Username] ->
			{irc, {msg, {RT, [P, io_lib:format("~s?nick=~s~s or ~s on irc.sorcery.net", [?BASE_URL, Username, RT, RT])]}}};
		_ ->
			{irc, {msg, {RT, [P, io_lib:format("~s~s or ~s on irc.sorcery.net", [?BASE_URL, RT, RT])]}}}
	end.

webchat(#{reply:=RT, ping:=P, params:=Params}) ->
	case Params of
		[Username] ->
			{irc, {msg, {RT, [P, io_lib:format("~s?nick=~s~s", [?BASE_URL, Username, RT])]}}};
		_ ->
			{irc, {msg, {RT, [P, io_lib:format("~s~s", [?BASE_URL, RT])]}}}
	end.

xkcd(#{reply:=RT, ping:=P, params:=Params}) ->
	os:putenv("xkcd", string:join(Params, " ")),
	Reply = util:safe_os_cmd("./xkcd.sh \"$xkcd\""),

	{irc, {msg, {RT, [P, Reply]}}}.
