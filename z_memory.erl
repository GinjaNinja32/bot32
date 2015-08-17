-module(z_memory).
-compile(export_all).

get_commands() ->
	[
		{"meminfo", fun meminfo/5, host}
	].

meminfo(_, RT, P, Params, _) ->
	Meminfo = os:cmd("cat /proc/meminfo"),
	Dict = lists:foldl(fun(Ln, Dict) ->
			case re:run(Ln, "^([^:]+):[\s\t]*(.+)", [{capture, all_but_first, list}]) of
				{match, [Key, Val]} -> orddict:store(Key, Val, Dict);
				T -> logging:log(error, "MEMORY", "Unable to parse line ~p (got ~p)", [Ln, T]), Dict
			end
		end, orddict:new(), string:tokens(Meminfo, "\n")),
%	common:debug("debug", "~p", [Dict]),
	case Params of
		[] -> showkeysto(RT, P, orddict:fetch_keys(Dict)), ok;
		_ ->
			Result = string:join(lists:map(fun(T) ->
				case orddict:find(T, Dict) of
					{ok, V} -> T ++ ": " ++ V;
					error -> T ++ ": ???"
				end
			end, Params), "; "),
			{irc, {msg, {RT, [P, Result]}}}
	end.

showkeysto(RT, P, Keys) when length(Keys) > 30 ->
	{FirstTwenty, Rest} = lists:split(30, Keys),
	core ! {irc, {msg, {RT, [P, string:join(FirstTwenty, " ")]}}},
	showkeysto(RT, P, Rest);
showkeysto(RT, P, Keys) ->
	core ! {irc, {msg, {RT, [P, string:join(Keys, " ")]}}},
	ok.
