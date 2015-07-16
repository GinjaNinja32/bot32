-module(z_status).
-compile(export_all).

get_commands() ->
	[
		{"status",      genstatus ("baystation12.net", 8000), user},
		{"players",     genplayers("baystation12.net", 8000), user},
		{"admins",      genadmins ("baystation12.net", 8000), user},
		{"mode",        genmode   ("baystation12.net", 8000), user},
		{"devstatus",   genstatus ("baystation12.net", 8100), user},
		{"devplayers",  genplayers("baystation12.net", 8100), user},
		{"devadmins",   genadmins ("baystation12.net", 8100), user},
		{"devmode",     genmode   ("baystation12.net", 8100), user},
		{"teststatus",  genstatus ("gn32.mooo.com",    3210), user},
		{"testplayers", genplayers("gn32.mooo.com",    3210), user},
		{"testadmins",  genadmins ("gn32.mooo.com",    3210), user},
		{"testmode",    genmode   ("gn32.mooo.com",    3210), user}
	].

initialise(T) -> T.
deinitialise(T) -> T.

genstatus (Server, Port) -> fun(_,RT,_,_,_) -> spawn(z_status, status,  [RT,Server,Port]), ok end.
genplayers(Server, Port) -> fun(_,RT,_,_,_) -> spawn(z_status, players, [RT,Server,Port]), ok end.
genadmins (Server, Port) -> fun(_,RT,_,_,_) -> spawn(z_status, admins,  [RT,Server,Port]), ok end.
genmode   (Server, Port) -> fun(_,RT,_,_,_) -> spawn(z_status, mode,    [RT,Server,Port]), ok end.

status(RT, S, P) ->
        case byond:send(S, P, "status") of
                {error, X} -> core ! {irc, {msg, {RT, io_lib:format("Error: ~p", [X])}}};
                Dict ->
                        Players = safeget(Dict, "players"),
                        Mode = safeget(Dict, "mode"),
                        Time = safeget(Dict, "stationtime"),
                        core ! {irc, {msg, {RT, ["Players: ", Players, "; Mode: ", Mode, "; Station Time: ", Time]}}}
        end.

admins(RT, S, P) ->
        case byond:send(S, P, "status") of
                {error, X} -> core ! {irc, {msg, {RT, io_lib:format("Error: ~p", [X])}}};
                Dict ->
                        Admins = safeget(Dict, "admins"),
                        core ! {irc, {msg, {RT, ["Admins: ", Admins]}}}
        end.

mode(RT, S, P) ->
        case byond:send(S, P, "status") of
                {error, X} -> core ! {irc, {msg, {RT, io_lib:format("Error: ~p", [X])}}};
                Dict ->
                        Mode = safeget(Dict, "mode"),
                        core ! {irc, {msg, {RT, ["Mode: ", Mode]}}}
        end.

players(RT, S, P) ->
        case byond:send(S, P, "status") of
                {error, X} -> core ! {irc, {msg, {RT, io_lib:format("Error: ~p", [X])}}};
                Dict ->
                        case safeget(Dict, "playerlist") of
                                "???" -> core ! {irc, {msg, {RT, "Players: Unknown!"}}};
				[] ->	core ! {irc, {msg, {RT, "No players present."}}};
                                PlayerList ->
					Ordered = lists:sort(lists:map(fun(N) -> re:replace(N, " ", [160], [{return, list}]) end, PlayerList)),
                                        {_, Str, _} = lists:foldl(fun acc_players/2, {0, [], RT}, lists:map(fun(T) -> [hd(T),160,tl(T)] end, Ordered)),
                                        core ! {irc, {msg, {RT, ["Players: ", string:join(lists:reverse(Str), ", ")]}}}
                        end
        end.

safeget(Dict, Key) ->
        case orddict:find(Key, Dict) of
                {ok, V} -> V;
                error -> "???"
        end.

acc_players(Name, {19, Acc, RT}) ->
        core ! {irc, {msg, {RT, ["Players: ",string:join(lists:reverse([Name|Acc]), ", ")]}}},
        {0, [], RT};
acc_players(Name, {Num, Acc, RT}) -> {Num+1, [Name|Acc], RT}.


