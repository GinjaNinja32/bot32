-module(status).
-compile(export_all).

-define(Sep, 16#feff).

defaultserver(T) ->
	case config:get_value(config, [?MODULE, default, T]) of
		'$none' -> config:require_value(config, [?MODULE, default, default]);
		X -> X
	end.

servers() -> config:require_value(config, [?MODULE, servers]).

get_commands() ->
	[
		{"address",  generic(address), user},
		{"status",   generic(status), user},
		{"players",  generic(players), user},
		{"admins",   generic(admins), user},
		{"mode",     generic(mode), user},
		{"manifest", generic(manifest), user}
	].

get_help("address") -> ["Get the address of the specified server." | help()];
get_help("status") -> ["Check the status of the specified server." | help()];
get_help("players") -> ["Get an online player list of the specified server." | help()];
get_help("admins") -> ["Get an online admin list of the specified server." | help()];
get_help("mode") -> ["Get the mode of the specified server." | help()];
get_help("manifest") -> ["Get the manifest of the specified server." | help()];
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
			{ok, {Addr,Port,Name}} -> spawn(status, Func, [RT, P, O, Addr, Port, Name]), ok;
			error -> {irc, {msg, {RT, [P, "Failed to find default server for this channel!"]}}}
		end;
	   (#{nick:=O,reply:=RT,ping:=P,params:=[ServerID]}) ->
		case orddict:find(ServerID, servers()) of
			{ok, {Addr,Port,Name}} -> spawn(status, Func, [RT, P, O, Addr, Port, Name]), ok;
			error -> {irc, {msg, {RT, [P, "Illegal argument!"]}}}
		end
	end.

address(RT, Ping, _, S, P, Name) ->
	core ! {irc, {msg, {RT, [Ping, Name, io_lib:format("byond://~s:~b", [S, P])]}}}.

status(RT, _, _, S, P, Name) ->
        case byond:send(S, P, "status=2") of
                {error, X} -> core ! {irc, {msg, {RT, io_lib:format("~sError: ~p", [Name, X])}}};
                Dict ->
                        Players = safeget(Dict, "players"),
                        Mode = safeget(Dict, "mode"),
                        Time = safeget(Dict, "stationtime"),
			Duration = safeget(Dict, "roundduration"),
                        core ! {irc, {msg, {RT, [Name, "Players: ", Players, "; Mode: ", Mode, "; Station Time: ", Time, "; Round Duration: ", Duration]}}}
	end.

admins(RT, _, _, S, P, Name) ->
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

mode(RT, _, _, S, P, Name) ->
        case byond:send(S, P, "status=2") of
                {error, X} -> core ! {irc, {msg, {RT, io_lib:format("~sError: ~p", [Name,X])}}};
                Dict ->
                        Mode = safeget(Dict, "mode"),
                        core ! {irc, {msg, {RT, [Name, "Mode: ", Mode]}}}
        end.

players(RT, _, _, S, P, Name) ->
	case byond:send(S, P, "status=2") of
		{error, X} -> core ! {irc, {msg, {RT, io_lib:format("~sError: ~p", [Name, X])}}};
		Dict ->
			case safeget(Dict, "players") of
				"?" -> core ! {irc, {msg, {RT, [Name, "Error."]}}};
				"0" -> core ! {irc, {msg, {RT, [Name, "No players present."]}}};
				_ ->
					Players = byond:params2dict(safeget(Dict, "playerlist")),
					Ordered = lists:sort(lists:map(fun({X,_}) -> re:replace(X, [32], <<160/utf8>>, [{return, binary}, global]) end, Players)),
					Binaried = lists:map(fun(<<A/utf8, B/binary>>) -> <<A/utf8, ?Sep/utf8, B/binary>> end, Ordered),
					{_, Str, _, _} = lists:foldl(fun acc_players/2, {0, hd(Binaried), RT, Name}, tl(Binaried)),
					core ! {irc, {msg, {RT, [Name, "Players: ", Str]}}}
			end
	end.

manifest(RT, _, O, S, P, Name) ->
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

acc_players(Name, {20, Acc, RT, SName}) ->
        core ! {irc, {msg, {RT, [SName, "Players: ", Acc]}}},
        {0, Name, RT, SName};
acc_players(Name, {Num, Acc, RT, SName}) -> {Num+1, <<Acc/binary, ", ", Name/binary>>, RT, SName}.


