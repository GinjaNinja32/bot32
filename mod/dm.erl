-module(dm).
-compile(export_all).

get_commands() ->
	[
		{"dm", fun dm/1, [{"string to evaluate", long}], user}
	].

get_help("dm") ->
	[
		"Use ;;; to separate global lines from proc lines.",
		"Use ;; to separate lines from each other; the last ;;-separated entry in the proc section will be inserted into a world.log << \"[]\" statement, unless it is blank.",
		"Use ; as a literal ; (eg in `if(x){y(); z()}`).",
		"Any code containing either ## or #include will not be executed for security reasons."
	];
get_help(_) -> unhandled.

dm(#{reply:=Reply, ping:=Ping, params:=[String]}) ->
	case re:run(String, "##|#include", [{capture,none}]) of
		match -> {irc, {msg, {Reply, [Ping, "You attempted to use either ## or #include; both are blocked for security reasons."]}}};
		nomatch ->
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

			file:write_file(["dm/", MD5, ".dme"], File),
			spawn(fun() ->
				Output = util:safe_os_cmd(["./dm_compile_run.sh ", MD5]),
				core ! {irc, {msg, {Reply, [Ping, string:join(string:tokens(Output, "\n"), ";  ")]}}}
			end),
			ok
	end.

file(PreLines, MainSetup, MainValue) ->
	[
		lists:map(fun(T) -> ["\n" | string:strip(T)] end, PreLines),
		"\n/world/New()\n\tdm_eval()\n\tdel(src)\n/proc/dm_eval()",
		lists:map(fun(T) -> ["\n\t" | string:strip(T)] end, MainSetup),
		case string:strip(MainValue) of
			[] -> "";
			SV -> ["\n\tworld.log << \"[", SV, "]\""]
		end
	].
