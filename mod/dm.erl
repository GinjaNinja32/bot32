-module(dm).
-compile(export_all).

get_commands() ->
	[
		{"dm", fun dm/1, [long], user}
	].

dm(#{reply:=Reply, ping:=Ping, params:=[String]}) ->
	Parts = re:split(String, ";;;", [{return, list}]),
	case Parts of
		[Main] -> Pre = [], ok;
		[Pre, Main] -> ok
	end,
	PreLines = re:split(Pre, ";;", [{return, list}]),

	MainLines = re:split(Main, ";;", [{return, list}]),
	MainSetup = lists:droplast(MainLines),
	MainValue = lists:last(MainLines),

	File = [
		lists:map(fun(T) -> ["\n" | string:strip(T)] end, PreLines),
		"\n/world/New()\n\tdm_eval()\n\tdel(src)\n/proc/dm_eval()",
		lists:map(fun(T) -> ["\n\t" | string:strip(T)] end, MainSetup),
		case string:strip(MainValue) of
			[] -> "";
			SV -> ["\n\tworld.log << \"[", SV, "]\""]
		end
	],
%	io:fwrite("~s\n", [File]),

	MD5 = re:replace(base64:encode(crypto:md5(File)), "/", "@", [global, {return, list}]),

	file:write_file(["dm/", MD5, ".dme"], File),
	spawn(fun() ->
		Output = util:safe_os_cmd(["./dm_compile_run.sh ", MD5]),
		core ! {irc, {msg, {Reply, [Ping, string:join(string:tokens(Output, "\n"), ";  ")]}}}
	end),
	ok.
