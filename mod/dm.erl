-module(dm).
-compile(export_all).

get_commands() ->
	[
		{"dm", fun dm/1, [{"string to evaluate", long}], user},
		{"dml", fun dml/1, [{"string to evaluate", long}], user},
		{"dms", fun dms/1, [{"string to evaluate", long}], eval},
		{"dmsl", fun dmsl/1, [{"string to evaluate", long}], eval}
	].

get_help("dm") ->
	[
		"Use ;;; to separate global lines from proc lines.",
		"Use ;; to separate lines from each other; the last ;;-separated entry in the proc section will be inserted into a world.log << \"[]\" statement, unless it is blank.",
		"Use ; as a literal ; (eg in `if(x){y(); z()}`).",
		"Any code containing either ## or include (whether in a string or not) will not be executed for security reasons."
	];
get_help("dml") ->
	[
		"Multi-line-output version of !dm"
	] ++ get_help("dm");
get_help(_) -> unhandled.

dms(#{reply:=Reply, ping:=Ping, params:=[String]}) ->
	dmrun(Reply, Ping, String, false, true).
dmsl(#{reply:=Reply, ping:=Ping, params:=[String]}) ->
	dmrun(Reply, Ping, String, true, true).
dm(#{reply:=Reply, ping:=Ping, params:=[String]}) ->
	dmrun(Reply, Ping, String, false, false).
dml(#{reply:=Reply, ping:=Ping, params:=[String]}) ->
	dmrun(Reply, Ping, String, true, false).

dmrun(Reply, Ping, InString, Multiline, Secure) ->
	String = case {hd(InString), lists:last(InString)} of
		{$`, $`} -> lists:droplast(tl(InString));
		_ -> InString
	end,
	case re:run(String, "##|include", [{capture,none}]) of
		match when not Secure -> {irc, {msg, {Reply, [Ping, "You attempted to use either ## or include; both are blocked for security reasons."]}}};
		_ ->
			Parts = re:split(String, ";;;", [{return, list}]),
			case Parts of
				[Main] -> Pre = [], ok;
				[Pre, Main] -> ok
			end,
			PreLines = re:split(Pre, ";;", [{return, list}]),

			MainLines = re:split(Main, ";;", [{return, list}]),
			MainSetup = lists:droplast(MainLines),
			MainValue = lists:last(MainLines),

			File = file(PreLines, MainSetup, MainValue),
%			io:fwrite("~s\n", [File]),

			MD5 = re:replace(base64:encode(crypto:hash(md5, File)), "/", "@", [global, {return, list}]),
			UseMD5 = lists:sublist(MD5, 9),

			file:write_file(["dm/", UseMD5, ".dme"], File),
			spawn(fun() ->
				util:unicode_os_putenv("multiline", if Multiline -> "true"; true -> "false" end),
				util:unicode_os_putenv("secure", if Secure -> "true"; true -> "false" end),
				Output = util:safe_os_cmd(["./dm_compile_run.sh ", UseMD5]),
				case string:tokens(Output, "\n") of
					[] -> core ! {irc, {msg, {Reply, [Ping, "<no output>"]}}};
					Lines ->
						lists:foreach(fun(Line) ->
								core ! {irc, {msg, {Reply, [Ping, Line]}}}
							end, Lines)
				end
			end),
			ok
	end.

file(PreLines, MainSetup, MainValue) ->
	[
		"#include \"util.dm\"\n",
		"/world/loop_checks = 0\n",
		lists:map(fun(T) -> ["\n" | string:strip(T)] end, PreLines),
		"\n/world/New()\n\tdm_eval()\n\tdel(src)\n/proc/dm_eval()",
		lists:map(fun(T) -> ["\n\t" | string:strip(T)] end, MainSetup),
		case string:strip(MainValue) of
			[] -> "";
			SV -> ["\n\tworld.log << \"[", SV, "]\""]
		end
	].
