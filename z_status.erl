-module(z_status).
-compile(export_all).

get_commands() ->
	[
		{"address",     genaddr   ("baystation12.net", 8000      ), user},
		{"status",      genstatus ("baystation12.net", 8000, true), user},
		{"players",     genplayers("baystation12.net", 8000, true), user},
		{"admins",      genadmins ("baystation12.net", 8000, true), user},
		{"mode",        genmode   ("baystation12.net", 8000, true), user},
		{"devaddress",  genaddr   ("baystation12.net", 8100      ), user},
		{"devstatus",   genstatus ("baystation12.net", 8100, true), user},
		{"devplayers",  genplayers("baystation12.net", 8100, true), user},
		{"devadmins",   genadmins ("baystation12.net", 8100, true), user},
		{"devmode",     genmode   ("baystation12.net", 8100, true), user},
		{"testaddress", genaddr   ("gn32.mooo.com",    3210       ), user},
		{"teststatus",  genstatus ("gn32.mooo.com",    3210, false), user},
		{"testplayers", genplayers("gn32.mooo.com",    3210, false), user},
		{"testadmins",  genadmins ("gn32.mooo.com",    3210, false), user},
		{"testmode",    genmode   ("gn32.mooo.com",    3210, false), user}
	].

initialise(T) -> T.
deinitialise(T) -> T.

genaddr   (Server, Port     ) -> fun(_,RT,P,_,_) -> {irc, {msg, {RT, [P, io_lib:format("byond://~s:~b", [Server, Port])]}}} end.
genstatus (Server, Port, New) -> fun(_,RT,_,_,_) -> spawn(z_status, status,  [RT,Server,Port,New]), ok end.
genplayers(Server, Port, New) -> fun(_,RT,_,_,_) -> spawn(z_status, players, [RT,Server,Port,New]), ok end.
genadmins (Server, Port, New) -> fun(_,RT,_,_,_) -> spawn(z_status, admins,  [RT,Server,Port,New]), ok end.
genmode   (Server, Port, New) -> fun(_,RT,_,_,_) -> spawn(z_status, mode,    [RT,Server,Port,New]), ok end.

status(RT, S, P, N) ->
        case byond:send(S, P, if N -> "status=2"; true -> "status" end) of
                {error, X} -> core ! {irc, {msg, {RT, io_lib:format("Error: ~p", [X])}}};
                Dict ->
                        Players = safeget(Dict, "players"),
                        Mode = safeget(Dict, "mode"),
                        Time = safeget(Dict, "stationtime"),
                        core ! {irc, {msg, {RT, ["Players: ", Players, "; Mode: ", Mode, "; Station Time: ", Time]}}}
	end.

admins(RT, S, P, N) ->
	if N ->
		case byond:send(S, P, "status=2") of
			{error, X} -> core ! {irc, {msg, {RT, io_lib:format("Error: ~p", [X])}}};
			Dict ->
				Msg = case byond:params2dict(safeget(Dict, "adminlist")) of
					[{"?",none}] -> "No admins online.";
					Admins ->
						BinMins = lists:map(fun({A,B}) -> {re:replace(A, <<32>>, <<160/utf8>>, [{return, binary}, global]),
						                                   re:replace(B, <<32>>, <<160/utf8>>, [{return, binary}, global])} end, Admins),
						AdminStr = util:binary_join(lists:map(fun({<<A/utf8,B/binary>>,C}) -> <<A/utf8, 160/utf8, B/binary, " is a ", C/binary>> end, BinMins), <<"; ">>),
						[io_lib:format("Admins (~b): ", [length(Admins)]), AdminStr]
				end,
				core ! {irc, {msg, {RT, Msg}}}
		end;
	true ->
		case byond:send(S, P, "status") of
	                {error, X} -> core ! {irc, {msg, {RT, io_lib:format("Error: ~p", [X])}}};
	                Dict ->
	                        Admins = safeget(Dict, "admins"),
	                        core ! {irc, {msg, {RT, ["Admins: ", Admins]}}}
		end
        end.

mode(RT, S, P, N) ->
        case byond:send(S, P, if N -> "status=2"; true -> "status" end) of
                {error, X} -> core ! {irc, {msg, {RT, io_lib:format("Error: ~p", [X])}}};
                Dict ->
                        Mode = safeget(Dict, "mode"),
                        core ! {irc, {msg, {RT, ["Mode: ", Mode]}}}
        end.

players(RT, S, P, N) ->
	if N ->
		case byond:send(S, P, "status=2") of
			{error, X} -> core ! {irc, {msg, {RT, io_lib:format("Error: ~p", [X])}}};
			Dict ->
				Players = byond:params2dict(safeget(Dict, "playerlist")),
				Ordered = lists:sort(lists:map(fun({X,_}) -> re:replace(X, [32], <<160/utf8>>, [{return, binary}, global]) end, Players)),
				Binaried = lists:map(fun(<<A/utf8, B/binary>>) -> <<A/utf8, 160/utf8, B/binary>> end, Ordered),
				{_, Str, _} = lists:foldl(fun acc_players/2, {0, hd(Binaried), RT}, tl(Binaried)),
				core ! {irc, {msg, {RT, ["Players: ", Str]}}}
		end;
	true ->
	        case byond:send(S, P, "status") of
			{error, X} -> core ! {irc, {msg, {RT, io_lib:format("Error: ~p", [X])}}};
			Dict ->
				Message = case safeget(Dict, "players") of
					"???" -> "Players: Unknown!";
					"0" -> "No players present.";
					Num ->
						PlayerList = lists:map(fun(T) -> safeget(Dict, "player" ++ integer_to_list(T)) end, lists:seq(0, list_to_integer(Num)-1)),
						Ordered = lists:sort(lists:map(fun(X) -> re:replace(X, [32], <<160/utf8>>, [{return, binary}, global]) end, PlayerList)),
						Binaried = lists:map(fun(<<A/utf8, B/binary>>) -> <<A/utf8, 160/utf8, B/binary>> end, Ordered),
						{_, Str, _} = lists:foldl(fun acc_players/2, {0, hd(Binaried), RT}, tl(Binaried)),
						["Players: ", Str]
				end,
				core ! {irc, {msg, {RT, Message}}}
		end
        end.

safeget(Dict, Key) ->
        case orddict:find(Key, Dict) of
                {ok, V} -> V;
                error -> "???"
        end.

acc_players(Name, {20, Acc, RT}) ->
        core ! {irc, {msg, {RT, ["Players: ", Acc]}}},
        {0, Name, RT};
acc_players(Name, {Num, Acc, RT}) -> {Num+1, <<Acc/binary, ", ", Name/binary>>, RT}.


