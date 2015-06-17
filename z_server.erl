-module(z_server).
-compile(export_all).

-include("definitions.hrl").
-define(Timer, 100).

get_commands() ->
	[
		{"pm", fun pm/5, server},
		{"notes", fun notes/5, server},
		{"notify", fun notify/5, user}
	].

initialise(T) ->
	case orddict:find(z_server, T#state.moduledata) of
		{ok, OldPid} when is_pid(OldPid) -> OldPid ! stop;
		_ -> ok
	end,
	case file:consult("server.crl") of
		{ok, [{Server, Port, Pwd}]} ->
			Pid = spawn(z_server, sloop, [Server, Port, 45678, Pwd]),
			set_data(T, Pid);
		{error, T} -> common:debug("SERVER", "~p", [T]), error
	end.

deinitialise(T) ->
	Pid = get_data(T),
	Pid ! stop,
	timer:sleep(25),
	T#state{moduledata=orddict:erase(z_server, T#state.moduledata)}.

get_data(S=#state{moduledata=M}) ->
	case orddict:find(z_server, M) of
		{ok, Value} -> Value;
		error ->
			common:debug("SERVER", "Pid not found, loading!"),
			NewState = initialise(S),
			self() ! {state, NewState},
			get_data(NewState)
	end.

set_data(S=#state{moduledata=M}, Data) ->
	S#state{moduledata=orddict:store(z_server, Data, M)}.

%

pm(_, RT, Ping, [], _) -> {irc, {msg, {RT, [Ping, "Provide a user to PM and a message."]}}};
pm(_, RT, Ping, [_], _) -> {irc, {msg, {RT, [Ping, "Provide a message."]}}};
pm(N, RT, Ping, Params, State) ->
	Pid = get_data(State),
	common:debug("SERVER", "debug ~w | ~w", [hd(Params), tl(Params)]),
	Pid ! {pm, N, RT, Ping, string:to_lower(hd(Params)), string:join(tl(Params), " ")},
	ok.

notes(_, RT, Ping, [], _) -> {irc, {msg, {RT, [Ping, "Provide a user to find notes for."]}}};
notes(N, RT, Ping, [Key], State) ->
	Pid = get_data(State),
	Pid ! {notes, N, RT, Ping, string:to_lower(Key)},
	ok;
notes(_, RT, Ping, _, _) -> {irc, {msg, {RT, [Ping, "Provide a single key."]}}}.

notify(N, RT, Ping, _, State) ->
	Pid = get_data(State),
	Pid ! {notify, N, RT, Ping},
	ok.

sloop(Svr, Prt, SPrt, Pwd) ->
	case gen_tcp:listen(SPrt, [list, {packet, http}, {active, false}]) of
		{ok, SvrSock} ->
			common:debug("SERVER", "Starting loop."),
			register(z_server, self()),
			loop(SvrSock, Svr, Prt, SPrt, Pwd, sets:new()),
			common:debug("SERVER", "Ending loop."),
			gen_tcp:close(SvrSock);
		{error, Reason} ->
			common:debug("SERVER", "Failed to open listen socket: ~p", [Reason])
	end.

loop(SvrSock, Svr, Prt, SPrt, Pwd, Notify) ->
	case receive
		{pm, Sender, ReplyChannel, ReplyPing, Recipient, Message} ->
			common:debug("SERVER", "sending PM ~s to ~s", [Message, Recipient]),
			Reply = case byond:send(Svr, Prt, io_lib:format("?adminmsg=~s;msg=~s;key=~s;sender=~s", lists:map(fun byond:vencode/1, [Recipient, Message, Pwd, Sender]))) of
				{error, T} -> io_lib:format("Error: ~s", [T]);
				Dict ->
					case orddict:fetch_keys(Dict) of
						["Message Successful"] -> "Sent.";
						[T] -> T
					end
			end,
			core ! {irc, {msg, {ReplyChannel, [ReplyPing, Reply]}}},
			ok;
		{notes, _, ReplyChannel, ReplyPing, Lookup} ->
			common:debug("SERVER", "finding notes for ~s", [Lookup]),
			Reply = case byond:send(Svr, Prt, io_lib:format("?notes=~s;key=~s", lists:map(fun byond:vencode/1, [Lookup, Pwd]))) of
				{error, T} -> io_lib:format("Error: ~s", [T]);
				Dict ->
					case hd(orddict:fetch_keys(Dict)) of
						"No information found on the given key." -> "No information found on the given key.";
						T ->
							os:putenv("sprunge", T),
							X = os:cmd("echo $sprunge | /home/nyx/bin/privatepaste.py --no-tee -e 1800"),
							case X of
								"Pasting...\n" ++ URL -> URL;
								_ -> ["unknown error: ", X]
							end
					end
			end,
			core ! {irc, {msg, {ReplyChannel, [ReplyPing, Reply]}}},
			ok;
		{notify, Who, ReplyChannel, ReplyPing} ->
			case sets:is_element(string:to_lower(Who), Notify) of
				true ->
					core ! {irc, {msg, {ReplyChannel, [ReplyPing, "You are already on the list to be notified!"]}}},
					ok;
				false ->
					core ! {irc, {msg, {ReplyChannel, [ReplyPing, "You will be notified when the server restarts."]}}},
					{notify, sets:add_element(string:to_lower(Who), Notify)}
			end;
		{notify, T} -> {notify, T};
		stop -> stop
	after
		?Timer ->
			case gen_tcp:accept(SvrSock, ?Timer) of
				{ok, Socket} -> readsock(Socket, Pwd, Notify), ok;
				{error, timeout} -> ok;
				{error, X} -> {error, X}
			end
	end of
		ok ->         loop(SvrSock, Svr, Prt, SPrt, Pwd, Notify);
		{notify,N} ->
			common:debug("SERVER", "setting notify as ~p", [sets:to_list(N)]),
			loop(SvrSock, Svr, Prt, SPrt, Pwd, N);
		stop -> ok;
		E -> common:debug("SERVER", "received unknown status ~p, exiting", [E])
	end.

readsock(Socket, Pwd, Notify) ->
	% timeout specified just to avoid crashing the bot if bad things happen
	case gen_tcp:recv(Socket, 0, 5000) of
		{ok, {http_request, 'GET', URL, _}} ->
			Plist = tl(URL), % a=x;b=y;c=z
			Dict = byond:params2dict(Plist),
			case orddict:find("pwd", Dict) of
				{ok, Pwd} ->
					case {orddict:find("chan", Dict), orddict:find("mesg", Dict)} of
						{{ok, Chan}, {ok, Mesg}} ->
							Msg = re:replace(Mesg, "[\r\n\t]+", " ", [global]),
							common:debug("SERVER", "relaying to ~s: ~s", [Chan, Msg]),
							case string:str(Msg, "Server starting up on ") of
								1 ->
									lists:foreach(fun(T) -> core ! {irc, {msg, {T, Msg}}} end, sets:to_list(Notify)),
									self() ! {notify, sets:new()};
								_ -> ok
							end,
							core ! {irc, {msg, {Chan, Msg}}};
						_ -> common:debug("SERVER", "received correctly keyed message with no channel and/or message")
					end;
				{ok, NotPwd} -> common:debug("SERVER", "received incorrect key ~s", [NotPwd]);
				error -> common:debug("SERVER", "received non-keyed message '~s'", [Plist])
			end,
			gen_tcp:send(Socket, "HTTP/1.1 200 OK\r\n\r\n"),
			gen_tcp:close(Socket);
		{error, T} -> common:debug("SERVER", "error in readsock: ~p", [T])
	end.
