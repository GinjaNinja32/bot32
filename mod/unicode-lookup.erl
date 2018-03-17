-module('unicode-lookup').
-export([get_commands/0]).

get_commands() ->
	[
		{"unicode", fun unicode/1, [short], user}
	].

unicode(#{reply:=Reply, ping:=Ping, params:=[Codepoint]}) ->
	CodeInt = case list_to_binary(Codepoint) of
		<<T/utf8>> when not ($0 =< T andalso T =< $9) andalso not (($A =< T andalso T =< $F) orelse ($a =< T andalso T =< $f))->
			os:putenv("codepoint", integer_to_list(T, 16)),
			T;
		_ ->
			os:putenv("codepoint", Codepoint),
			list_to_integer(Codepoint, 16)
	end,
	Msg = util:safe_os_cmd(<<"curl -so- \"https://codepoints.net/U+$codepoint\" | grep -Eo '<title>.* . Codepoints</title>' | sed -r 's|<title>||; s| . Codepoints</title>||'">>),
	{irc, {msg, {Reply, [Ping, Msg, ": ", <<CodeInt/utf8>>]}}}.
