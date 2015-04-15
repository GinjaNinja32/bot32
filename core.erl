-module(core).
-compile(export_all).

-define(SOCK_OPTIONS, [list, {packet, line}, {active, true}]).

init() ->
	case file:consult("core_config.crl") of
		{ok, [{Server, Port}]} -> init(Server, Port);
		_ -> common:debug("CORE", "Failed to read config file!")
	end.

init(Server, Port) ->
	{ok, Sock} = gen_tcp:connect(Server, Port, ?SOCK_OPTIONS),
	register(core, self()),
	common:debug("CORE", "starting"),
	loop(Sock),
	common:debug("CORE", "quitting"),
	case whereis(bot) of
		undefined -> ok;
		Pid -> Pid ! quit, ok
	end.

loop(Sock) ->
	case receive
		{tcp, Sock, RawMessage} ->
			Message = lists:reverse(tl(tl(lists:reverse(RawMessage)))),
			case common:tcp_parse(Sock, Message) of
				{irc, T} ->
					case whereis(bot) of
						undefined ->
								case T of
									{msg, {_User, _Channel, ["##SPAWN"]}} -> common:debug("CORE", "Spawn request received, spawning!"), spawn(bot,init,[]);
									_ -> common:debug("CORE", "could not find bot PID!")
								end;
						Pid -> Pid ! {irc, T}, ok
					end;
				T when is_atom(T) -> T;
				T -> common:debug("CORE", "unknown return value ~p from tcp_parse", [T])
			end;
		{tcp_closed, Sock} ->
			common:debug("CORE", "Socket closed, attempting reconnect..."),
			case try_reconnect(Sock) of
				{error, T} -> common:debug("CORE", "Could not reconnect, exiting (~p)", [T]), quit;
				{ok, New} -> common:debug("CORE", "Regained connection."), loop(New)
			end;
		{raw, Message} -> common:raw_send(Sock, Message), ok;
		{irc, Message} -> common:tcp_send(Sock, Message), ok;
		T when is_atom(T) -> T
	end of
		quit -> ok;
		error -> error;
		ok -> loop(Sock);
		update -> core:loop(Sock);
		S ->
			common:debug("CORE", "unknown message ~p, continuing", [S]),
			loop(Sock)
	end.

try_reconnect(OldSock) ->
	case inet:peernames(OldSock) of
		{error, P} -> {error, P};
		{ok, [{Address, Port}]} -> try_reconnect(Address, Port, 5);
		T -> common:debug("CORE", "Reconnect failed to get address: ~p", [T])
	end.

try_reconnect(Ip, Port, Iterations) ->
	if
		Iterations == 0 -> {error, couldnotconnect};
		true ->
			case gen_tcp:connect(Ip, Port, ?SOCK_OPTIONS) of
				{ok, Sock} -> Sock;
				{error, _} -> timer:sleep(2000), try_reconnect(Ip, Port, Iterations-1);
				T -> common:debug("CORE", "Reconnect received ~p, exiting", [T]), {error, {unexpected, T}}
			end
	end.
