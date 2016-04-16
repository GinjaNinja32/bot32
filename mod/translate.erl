-module(translate).
-compile(export_all).

get_commands() ->
	[
		{"translate", fun translate/1, [short, short, long], user}
	].

fetch(Src, Dst, Query) ->
	os:putenv("srclang", Src),
	os:putenv("dstlang", Dst),
	os:putenv("query", http_uri:encode(Query)),

	Reply = util:safe_os_cmd("curl -s --user-agent Chrome/49 \"https://translate.googleapis.com/translate_a/single?client=gtx&sl=$srclang&tl=$dstlang&dt=t&q=$query\""),

	case re:run(Reply, "\\[\\[\\[\"([^\"]+)\",\"([^\"]+)\".*", [{capture, all_but_first, list}]) of
		{match, [Final, Pre]} ->
			BinFinal = list_to_binary(Final),
			io:fwrite("~ts -> [~ts|~ts]\n", [Reply, BinFinal, Pre]),
			BinFinal;
		X ->
			io:fwrite("~ts -> ~tp\n", [Reply, X]),
			"Unknown error."
	end.

translate(#{reply:=Reply, ping:=Ping, params:=[Src,Dst,Query]}) ->
	{irc, {msg, {Reply, [Ping, fetch(Src, Dst, Query)]}}}.
