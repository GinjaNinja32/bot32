-module(dm).
-compile(export_all).

get_commands() ->
	[
		{"dm", fun dm/1, [long], user}
	].

dm(#{reply:=Reply, ping:=Ping, params:=[String]}) ->
	X = re:split(String, ";;", [{return, list}]),
	Setup = lists:droplast(X),
	Value = lists:last(X),

	File = [
		"\n/world/New()\n\tdm_eval()\n\tdel(src)\n/proc/dm_eval()",
		lists:map(fun(T) -> ["\n\t" | string:strip(T)] end, Setup),
		case string:strip(Value) of
			[] -> "";
			SV -> ["\n\tworld.log << \"[", SV, "]\""]
		end
	],

	file:write_file("dm.dme", File),
	spawn(fun() ->
		Output = os:cmd("./dm_compile_run.sh"),
		core ! {irc, {msg, {Reply, [Ping, string:join(string:tokens(Output, "\n"), ";  ")]}}}
	end),
	ok.
