-module(translate).
-compile(export_all).

get_commands() ->
	[
		{"translate", fun translate/1, [short, short, long], user}
	].

fetch(Src, Dst, Query) ->
	util:unicode_os_putenv("srclang", Src),
	util:unicode_os_putenv("dstlang", Dst),
	util:unicode_os_putenv("query", re:replace(Query, " ", "%20", [global])),
	util:safe_os_cmd("echo \"$query\" > ~/query"),
	Reply = util:safe_os_cmd("curl -s --user-agent \"Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/50.0.2661.75 Safari/537.36\" \"https://translate.googleapis.com/translate_a/single?client=gtx&sl=$srclang&tl=$dstlang&dt=t&q=$query\""),
	io:fwrite("~s\n", [Reply]),

	case re:run(Reply, "\\[\\[\\[\"([^\"]+)\",\"([^\"]+)\".*", [{capture, all_but_first, binary}]) of
		{match, [BinFinal, Pre]} ->
			io:fwrite("~w -> [~ts|~ts]\n", [Reply, BinFinal, Pre]),
			BinFinal;
		X ->
			io:fwrite("~w -> ~tp\n", [list_to_binary(Reply), X]),
			"Unknown error."
	end.

translate(#{reply:=Reply, ping:=Ping, params:=[Src,Dst,Query]}) ->
	{irc, {msg, {Reply, [Ping, fetch(Src, Dst, Query)]}}}.
