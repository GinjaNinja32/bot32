-module('unicode-lookup').
-export([get_commands/0]).

get_commands() ->
	[
		{"unicode", fun unicode/1, [short], user}
	].

unicode(#{reply:=Reply, ping:=Ping, params:=[Codepoint]}) ->
	CodeInt = list_to_integer(Codepoint, 16),
	os:putenv("codepoint", Codepoint),
	Msg = util:safe_os_cmd(<<"curl -so- \"https://codepoints.net/U+$codepoint\" | grep -Eo '<title>.* . Codepoints</title>' | sed -r 's|<title>||; s| . Codepoints</title>||'">>),
	{irc, {msg, {Reply, [Ping, Msg, ": ", <<CodeInt/utf8>>]}}}.
