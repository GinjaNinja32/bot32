-module(translate).
-compile(export_all).

get_commands() ->
	[
		{"translate", fun translate/1, user}
	].

initialise() ->
	LangStr = os:cmd("./get_langs.sh 2>/dev/null"),

	Langs = lists:map(fun(T) -> list_to_tuple(string:tokens(T, "\t")) end, string:tokens(LangStr, "\n")),

	config:set_value(temp, [?MODULE, languages], Langs).

fetch(Src, Dst, Query) ->
	case {validate_language(Src), validate_language(Dst)} of
		{false, _} -> io_lib:format("Unknown/invalid language key ~s; try 'translate languages' for a list of supported languages", [Src]);
		{_, false} -> io_lib:format("Unknown/invalid language key ~s; try 'translate languages' for a list of supported languages", [Dst]);
		{true, true} ->
			util:unicode_os_putenv("srclang", Src),
			util:unicode_os_putenv("dstlang", Dst),
			Qry = lists:flatmap(fun(T) ->
				if
					T =< 32 orelse 127 =< T -> [$% | integer_to_list(T, 16)];
					true -> [T]
				end
			end, Query),
			util:unicode_os_putenv("query", Qry),
			Reply = util:safe_os_cmd("curl -s --user-agent \"Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/50.0.2661.75 Safari/537.36\" \"https://translate.google.com/translate_a/single?client=gtx&sl=$srclang&tl=$dstlang&dt=t&q=$query\""),
			JSON = json:parse(Reply),
			% logging:log(info, ?MODULE, "~s:~tp => ~s:~tp", [Src, Query, Dst, JSON]),
			util:fix_utf8(lists:map(fun({array,[S|_]}) -> S end, json:traverse(JSON, [array, array])))
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

validate_language(Code) ->
	lists:keymember(Code, 1, config:get_value(temp, [?MODULE, languages])).

get_languages() ->
	lists:map(fun({Key,Name}) ->
			case string:to_lower(string:substr(Name, 1, 2)) of
				Key -> Name;
				_ -> [Name, " [", Key, "]"]
			end
		end, config:get_value(temp, [?MODULE, languages])).
