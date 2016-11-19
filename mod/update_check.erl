-module(update_check).
-compile(export_all).

get_commands() ->
	[
		{"update?", fun checkupdate/1, user}
	].

checkupdate(#{reply:=Reply, ping:=Ping}) ->
	case byond:send("baystation12.net", 8000, "revision") of
		{error, X} -> {irc, {msg, {Reply, [Ping, io_lib:format("Error: ~p", [X])]}}};
		Dict ->
			case orddict:find("revision", Dict) of
				error -> {irc, {msg, {Reply, [Ping, "Server did not reply with a valid revision"]}}};
				{ok, Rev} ->
					os:putenv("rev", Rev),
					{irc, {msg, {Reply, [Ping, util:safe_os_cmd("bash -c './getrev.sh \"$rev\"'")]}}}
			end
	end.
