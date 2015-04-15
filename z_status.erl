-module(z_status).
-compile(export_all).

get_commands() ->
	[
		{"status",   genstatus ("baystation12.net", 8000), user},
		{"players",  genplayers("baystation12.net", 8000), user},
		{"admins",   genadmins ("baystation12.net", 8000), user}
	].

initialise(T) -> T.
deinitialise(_) -> ok.

genstatus (Server, Port) -> fun(_,RT,_,_,_) -> status (RT,Server,Port) end.
genplayers(Server, Port) -> fun(_,RT,_,_,_) -> players(RT,Server,Port) end.
genadmins (Server, Port) -> fun(_,RT,_,_,_) -> players(RT,Server,Port) end.

status(RT, S, P) ->
        case byond:send(S, P, "status") of
                {error, X} -> {irc, {msg, {RT, io_lib:format("Error: ~p", [X])}}};
                Dict ->
                        Players = safeget(Dict, "players"),
                        Mode = safeget(Dict, "mode"),
                        Time = safeget(Dict, "stationtime"),
                        {irc, {msg, {RT, ["Players: ", Players, "; Mode: ", Mode, "; Station Time: ", Time]}}}
        end.

admins(RT, S, P) ->
        case byond:send(S, P, "status") of
                {error, X} -> {irc, {msg, {RT, io_lib:format("Error: ~p", [X])}}};
                Dict ->
                        Admins = safeget(Dict, "admins"),
                        {irc, {msg, {RT, ["Admins: ", Admins]}}}
        end.

players(RT, S, P) ->
        case byond:send(S, P, "status") of
                {error, X} -> {irc, {msg, {RT, io_lib:format("Error: ~p", [X])}}};
                Dict ->
                        PlayerList = safeget(Dict, "playerlist"),
                        case PlayerList of
                                "???" -> {irc, {msg, {RT, "Players: Unknown!"}}};
                                _ ->
                                        {_, Str, _} = lists:foldl(fun acc_players/2, {0, [], RT}, lists:map(fun(T) -> [hd(T)," ",tl(T)] end, lists:sort(PlayerList))),
                                        {irc, {msg, {RT, ["Players: ", string:join(lists:reverse(Str), ", ")]}}}
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


