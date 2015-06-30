-module(core).
-compile(export_all).

-define(SOCK_OPTIONS, [list, {packet, line}, {active, true}]).
-include("definitions.hrl").

init() ->
	case file:consult("core_config.crl") of
		{ok, [{Server, Port}]} -> init(Server, Port);
		{ok, Terms} -> logging:log(error, "CORE", "Wrong config file format: ~p", [Terms]);
		{error, R} -> logging:log(error, "CORE", "Failed to read config file: ~p", [R])
	end.

init(Server, Port) ->
	case ?TRANSPORT of
		ssl -> ssl:start();
		_ -> ok
	end,
	{ok, Sock} = ?TRANSPORT:connect(Server, Port, ?SOCK_OPTIONS),
	register(core, self()),
	logging:log(info, "CORE", "starting"),
	loop(Sock),
	logging:log(info, "CORE", "quitting"),
	?TRANSPORT:close(Sock),
	case ?TRANSPORT of
		ssl -> ssl:stop();
		_ -> ok
	end,
	case whereis(bot) of
		undefined -> ok;
		Pid -> Pid ! quit, ok
	end.

loop(Sock) ->
	case receive
		{ssl, Sock, RawMessage} -> self() ! {tcp, Sock, RawMessage}, ok;
		{ssl_closed, Sock} -> self() ! {tcp_closed, Sock}, ok;
		{ssl_error, Sock, Reason} -> self() ! {tcp_error, Sock, Reason}, ok;
		{tcp, Sock, RawMessage} ->
			Message = lists:reverse(tl(tl(lists:reverse(RawMessage)))),
			case common:tcp_parse(Sock, Message) of
				{irc, T} ->
					case whereis(bot) of
						undefined ->
								case T of
									{msg, {_User, _Channel, ["##SPAWN"]}} -> logging:log(info, "CORE", "Spawn request received, spawning!"), spawn(bot,init,[]), ok;
									_ -> logging:log(error, "CORE", "could not find bot PID!")
								end;
						Pid -> Pid ! {irc, T}, ok
					end;
				T when is_atom(T) -> T;
				T -> logging:log(error, "CORE", "unknown return value ~p from tcp_parse", [T])
			end;
		{tcp_closed, Sock} -> logging:log(error, "CORE", "Socket closed, quitting"), quit;
		{tcp_error, Sock, Reason} -> logging:log(error, "CORE", "Socket error: ~p", [Reason]), quit;
		{raw, Message} -> common:raw_send(Sock, Message), ok;
		{irc, Message} -> common:tcp_send(Sock, Message), ok;
		T when is_atom(T) -> T;
		X -> logging:log(error, "CORE", "Received unknown message ~p", [X])
	end of
		quit -> ok;
		error -> error;
		ok -> loop(Sock);
		update -> core:loop(Sock);
		S ->
			logging:log(error, "CORE", "unknown message ~p, continuing", [S]),
			loop(Sock)
	end.

try_reconnect(OldSock) ->
	case inet:peernames(OldSock) of
		{error, P} -> {error, P};
		{ok, [{Address, Port}]} -> try_reconnect(Address, Port, 5);
		T -> logging:log(error, "CORE", "Reconnect failed to get address: ~p", [T])
	end.

try_reconnect(Ip, Port, Iterations) ->
	if
		Iterations == 0 -> {error, couldnotconnect};
		true ->
			case ?TRANSPORT:connect(Ip, Port, ?SOCK_OPTIONS) of
				{ok, Sock} -> Sock;
				{error, _} -> timer:sleep(2000), try_reconnect(Ip, Port, Iterations-1);
				T -> logging:log(error, "CORE", "Reconnect received ~p, exiting", [T]), {error, {unexpected, T}}
			end
	end.
