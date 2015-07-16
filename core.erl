-module(core).
-compile(export_all).

-define(SOCK_OPTIONS, [list, {packet, line}, {active, true}]).
-include("definitions.hrl").

init() ->
	case file:consult("core_config.crl") of
		{ok, [{Server, Port, Transport}]} -> init(Server, Transport, Port);
		{ok, Terms} -> logging:log(error, "CORE", "Wrong config file format: ~p", [Terms]);
		{error, R} -> logging:log(error, "CORE", "Failed to read config file: ~p", [R])
	end.

init(Server, Transport, Port) ->
	case Transport of
		ssl -> ssl:start();
		_ -> ok
	end,
	{ok, Sock} = Transport:connect(Server, Port, ?SOCK_OPTIONS),
	register(core, self()),
	logging:log(info, "CORE", "starting"),
	loop(Sock, Transport),
	logging:log(info, "CORE", "quitting"),
	Transport:close(Sock),
	case Transport of
		ssl -> ssl:stop();
		_ -> ok
	end,
	case whereis(bot) of
		undefined -> ok;
		Pid -> Pid ! quit, ok
	end.

loop(Sock, Transport) ->
	case receive
		{ssl, Sock, RawMessage} -> self() ! {tcp, Sock, RawMessage}, ok;
		{ssl_closed, Sock} -> self() ! {tcp_closed, Sock}, ok;
		{ssl_error, Sock, Reason} -> self() ! {tcp_error, Sock, Reason}, ok;
		{tcp, Sock, RawMessage} ->
			Message = lists:reverse(tl(tl(lists:reverse(RawMessage)))),
			case common:tcp_parse(Sock, Transport, Message) of
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
		{raw, Message} -> common:raw_send(Sock, Transport, Message), ok;
		{irc, Message} -> common:tcp_send(Sock, Transport, Message), ok;
		T when is_atom(T) -> T;
		X -> logging:log(error, "CORE", "Received unknown message ~p", [X])
	end of
		quit -> ok;
		error -> error;
		ok -> loop(Sock, Transport);
		update -> core:loop(Sock, Transport);
		S ->
			logging:log(error, "CORE", "unknown message ~p, continuing", [S]),
			loop(Sock, Transport)
	end.
