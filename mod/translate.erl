-module(translate).
-compile(export_all).

get_commands() ->
	[
		{"translate", fun translate/1, user}
	].

initialise() ->
	LangStr = os:cmd("./get_langs.sh"),

	Langs = lists:map(fun(T) -> list_to_tuple(string:tokens(T, "\t")) end, string:tokens(LangStr, "\n")),

	config:set_value(temp, [?MODULE, languages], Langs).

fetch(Src, Dst, Query) ->
	util:unicode_os_putenv("srclang", Src),
	util:unicode_os_putenv("dstlang", Dst),
	util:unicode_os_putenv("query", re:replace(Query, " ", "%20", [global])),
	Reply = util:safe_os_cmd("curl -s --user-agent \"Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/50.0.2661.75 Safari/537.36\" \"https://translate.google.com/translate_a/single?client=gtx&sl=$srclang&tl=$dstlang&dt=t&q=$query\""),

	case re:run(Reply, "\\[\\[\\[\"([^\"]+)\",\"(?:[^\"]+)\".*", [{capture, all_but_first, binary}]) of
		{match, [BinFinal]} ->
			BinFinal;
		X ->
			logging:log(info, ?MODULE, "regex match got ~w -> ~tp\n", [list_to_binary(Reply), X]),
			"Unknown error."
	end.

translate(#{reply:=Reply, ping:=Ping, params:=Params}) ->
	case Params of
		[Src, Dst | Query] when Query /= [] ->
			{irc, {msg, {Reply, [Ping, fetch(Src, Dst, string:join(Query, " "))]}}};
		["languages"] ->
			util:groupstrs(fun(T) -> core ! {irc, {msg, {Reply, [Ping, T]}}} end, 200, ["Languages supported by this command (key is first two letters unless specified in square brackets)" | get_languages()], "; "),
			ok;
		_ ->
			core ! {irc, {msg, {Reply, [Ping, "Usage: translate <source language> <destination language> <string to translate>"]}}},
			core ! {irc, {msg, {Reply, [Ping, "For a list of supported languages, try: translate languages"]}}},
			ok
	end.

get_languages() ->
	lists:map(fun({Key,Name}) ->
			case string:to_lower(string:substr(Name, 1, 2)) of
				Key -> Name;
				_ -> [Name, " [", Key, "]"]
			end
		end, config:get_value(temp, [?MODULE, languages])).
