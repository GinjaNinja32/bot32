-module(status).
-compile(export_all).

-define(Sep, 16#feff).

do_extras(Tokens, Reply, _) ->
	case lists:dropwhile(fun(X) -> re:run(X, "^byond://.*$", [{capture, none}]) /= match end, Tokens) of
		[] -> ok;
		[IP|_] ->
			case re:run(IP, "^byond://([^:]+)(?::([1-9][0-9]*))$", [{capture, all_but_first, list}]) of
				nomatch -> ok;
				{match, [Addr, Port]} ->
					status(Reply, Addr, list_to_integer(Port), [$(,IP,$),$ ], true)
			end
	end.

defaultserver(T) ->
	case config:get_value(config, [?MODULE, default, T]) of
		'$none' -> config:require_value(config, [?MODULE, default, default]);
		X -> X
	end.

servers() -> config:require_value(config, [?MODULE, servers]).

canonicalise(ID) ->
	case config:get_value(config, [?MODULE, alias, ID]) of
		'$none' -> ID;
		T -> T
	end.

get_commands() ->
	[
		{"address",  generic(address), user},
		{"status",   generic(status), user},
		{"players",  generic(players), user},
		{"admins",   generic(admins), user},
		{"mode",     generic(mode), user},
		{"manifest", generic(manifest), user},
		{"revision", generic(revision), user},
		{"update?",  generic(check_update), user}
	].

get_help("address") -> ["Get the address of the specified server." | help()];
get_help("status") -> ["Check the status of the specified server." | help()];
get_help("players") -> ["Get an online player list of the specified server." | help()];
get_help("admins") -> ["Get an online admin list of the specified server." | help()];
get_help("mode") -> ["Get the mode of the specified server." | help()];
get_help("manifest") -> ["Get the manifest of the specified server." | help()];
get_help("revision") -> ["Get the revision of the specified server." | help()];
get_help(_) -> unhandled.

help() ->
	[
		["Valid servers are: ",
		string:join(lists:map(fun({ID, {Addr,Port,_Name}}) ->
				io_lib:format("'~s' (~s:~b)", [ID, Addr, Port])
			end, servers()), "; ")]
	].

generic(Func) ->
	fun(#{nick:=O,reply:=RT,ping:=P,params:=[]}) ->
		case orddict:find(defaultserver(RT), servers()) of
			{ok, {Addr,Port,Name}} -> spawn(status, Func, [RT, P, O, Addr, Port, defaultserver(RT), Name]), ok;
			error -> {irc, {msg, {RT, [P, "Failed to find default server for this channel!"]}}}
		end;
	   (#{nick:=O,reply:=RT,ping:=P,params:=[Address="byond://"++_]}) ->
		case re:run(Address, "^byond://([^:]+)(?::([1-9][0-9]*))$", [{capture, all_but_first, list}]) of
			{match,[IP,Port]} ->
				spawn(status, Func, [RT, P, O, IP, list_to_integer(Port), Address, [$(,Address,$),$ ]]);
			_ -> {irc, {msg, {RT, [P, "Illegal argument!"]}}}
		end;
	   (#{nick:=O,reply:=RT,ping:=P,params:=[ServerID]}) ->
		case orddict:find(canonicalise(ServerID), servers()) of
			{ok, {Addr,Port,Name}} -> spawn(status, Func, [RT, P, O, Addr, Port, canonicalise(ServerID), Name]), ok;
			error -> {irc, {msg, {RT, [P, "Illegal argument!"]}}}
		end;
	   (#{reply:=RT,ping:=P}) ->
		{irc, {msg, {RT, [P, "Please provide a single server ID or byond:// URL argument!"]}}}
	end.

address(RT, Ping, _, S, P, _, Name) ->
	core ! {irc, {msg, {RT, [Ping, Name, io_lib:format("byond://~s:~b", [S, P])]}}}.

status(RT, _, _, S, P, _, Name) ->
	status(RT, S, P, Name, false).
status(RT, S, P, Name, SilenceErrors) ->
	case byond:send(S, P, "status=2") of
		{error, _} when SilenceErrors -> ok;
		{error, X} -> core ! {irc, {msg, {RT, io_lib:format("~sError: ~p", [Name, X])}}};
		Dict ->
			Display = [{"Players", "players"}, {"Active Players", "active_players"}, {"Mode", "mode"}, {"Station Time", "stationtime"}, {"Round Duration", "roundduration"}, {"Map", "map"}],
			DispParts = lists:filtermap(fun({Disp,Key}) ->
				case orddict:find(Key, Dict) of
					{ok, V} -> {true, [Disp, ": ", V]};
					error -> false
				end end, Display),
			core ! {irc, {msg, {RT, [Name, string:join(DispParts, "; ")]}}}
	end.

revision(RT, _, _, S, P, ID, Name) ->
	case byond:send(S, P, "revision") of
		{error, X} -> core ! {irc, {msg, {RT, io_lib:format("~sError: ~p", [Name, X])}}};
		[{"unknown","?"}] -> core ! {irc, {msg, {RT, io_lib:format("~sRevision unknown.", [Name])}}};
		Dict ->
			Branch = safeget(Dict, "branch"),
			Date = safeget(Dict, "date"),
			Rev = safeget(Dict, "revision"),
			GID = safeget(Dict, "gameid"),
			DDV = safeget(Dict, "dd_version"),
			DDB = safeget(Dict, "dd_build"),
			DMV = safeget(Dict, "dm_version"),
			DMB = safeget(Dict, "dm_build"),
			Str = io_lib:format("Game ID: ~s. DM: ~s.~s; DD: ~s.~s", [GID, DMV, DMB, DDV, DDB]),
			Msg = case config:get_value(config, [?MODULE, github, ID]) of
				'$none' -> io_lib:format("~sRevision: ~s on ~s at ~s. ~s", [Name, Rev, Branch, Date, Str]);
				URL -> io_lib:format("~sRevision: ~s on ~s at ~s: ~s. ~s", [Name, lists:sublist(Rev, 8), Branch, Date, [URL,Rev], Str])
			end,
			core ! {irc, {msg, {RT, Msg}}}
	end.

admins(RT, _, _, S, P, _, Name) ->
	case byond:send(S, P, "status=2") of
		{error, X} -> core ! {irc, {msg, {RT, io_lib:format("~sError: ~p", [Name,X])}}};
		Dict ->
			Msg = case byond:params2dict(safeget(Dict, "adminlist")) of
				[{"?",none}] -> [Name,"No admins online."];
				Admins ->
					BinMins = lists:map(fun({A,B}) -> {re:replace(A, <<32>>, <<160/utf8>>, [{return, binary}, global]),
					                                   re:replace(B, <<32>>, <<160/utf8>>, [{return, binary}, global])} end, Admins),
					AdminStr = util:binary_join(lists:map(fun({<<A/utf8,B/binary>>,C}) -> CA=a(C), <<A/utf8, ?Sep/utf8, B/binary, " is ", CA/binary, " ", C/binary>> end, BinMins), <<"; ">>),
					[io_lib:format("~sAdmins (~b): ", [Name,length(Admins)]), AdminStr]
			end,
			core ! {irc, {msg, {RT, Msg}}}
	end.

a(<<T/utf8, _/binary>>) ->
	case lists:member(T, "AEIOUaeiou") of
		true -> <<"an">>;
		false -> <<"a">>
	end;
a(_) -> <<"a">>.

mode(RT, _, _, S, P, _, Name) ->
	case byond:send(S, P, "status=2") of
		{error, X} -> core ! {irc, {msg, {RT, io_lib:format("~sError: ~p", [Name,X])}}};
		Dict ->
			Mode = safeget(Dict, "mode"),
			core ! {irc, {msg, {RT, [Name, "Mode: ", Mode]}}}
	end.

players(RT, _, _, S, P, _, Name) ->
	case byond:send(S, P, "status=2") of
		{error, X} -> core ! {irc, {msg, {RT, io_lib:format("~sError: ~p", [Name, X])}}};
		Dict ->
			case safeget(Dict, "players") of
				"?" -> core ! {irc, {msg, {RT, [Name, "Error."]}}};
				"0" -> core ! {irc, {msg, {RT, [Name, "No players present."]}}};
				_ ->
					Players = byond:params2dict(safeget(Dict, "playerlist")),
					Ordered = lists:sort(lists:map(fun({X,_}) -> re:replace(X, [32], <<160/utf8>>, [{return, binary}, global]) end, Players)),
					Names = lists:map(fun(<<A/utf8, B/binary>>) -> binary_to_list(<<A/utf8, ?Sep/utf8, B/binary>>) end, Ordered),
					util:groupstrs(fun(T) -> core!{irc,{msg,{RT,[Name, "Players (", integer_to_list(length(Names)), "): ", T]}}} end, 250, Names, ", ")
			end
	end.

manifest(RT, _, O, S, P, _, Name) ->
	case byond:send(S, P, "manifest") of
		{error, X} -> core ! {irc, {msg, {RT, io_lib:format("~sError: ~p", [Name, X])}}};
		[] -> core ! {irc, {msg, {RT, [Name, "Manifest is empty"]}}};
		Dict ->
			Size = lists:map(fun({Dept,Players}) ->
					Manif = lists:map(fun({K,V}) -> [K, ": ", V] end, byond:params2dict(Players)),
					core ! {irc, {msg, {O, [Name, Dept, $:, $ , string:join(Manif, "; ")]}}},
					io_lib:format("~s: ~b", [Dept, length(Manif)])
				end, Dict),
			core ! {irc, {msg, {RT, [Name, "Manifest lengths: ", string:join(Size, "; ")]}}}
	end.

safeget(Dict, Key) ->
	case orddict:find(Key, Dict) of
		{ok, V} -> V;
		error -> "???"
	end.

% UPDATE CHECKER

check_update(RT, _, _, S, P, ID, Name) ->
	case config:get_value(config, [?MODULE, repobase, ID]) of
		'$none' ->
			io:fwrite("~w\n", [ID]),
			core ! {irc, {msg, {RT, "I don't have a repo configured for that server!"}}};
		Repo ->
			Branch = config:require_value(config, [?MODULE, repobranch, ID]),
			Github = config:require_value(config, [?MODULE, repogithub, ID]),
			case byond:send(S, P, "revision") of
				{error, X} -> core ! {irc, {msg, {RT, io_lib:format("~sError: ~p", [Name, X])}}};
				Dict ->
					case orddict:find("revision", Dict) of
						error -> core ! {irc, {msg, {RT, [P, "Server did not reply with a valid revision."]}}};
						{ok, Rev} ->
							os:putenv("rev", Rev),
							os:putenv("repo", Repo),
							os:putenv("branch", Branch),
							os:putenv("github", Github),
							Reply = util:safe_os_cmd("bash -c './getrev.sh \"$rev\" \"$repo\" \"$branch\" \"$github\"'"),
							core ! {irc, {msg, {RT, Reply}}}
					end
			end
	end.

