-module(server).
-compile(export_all).

-include("definitions.hrl").
-define(Timer, 100).

waitfor_gone(Ident) ->
	case whereis(Ident) of
		undefined -> ok;
		_ ->
			timer:sleep(100),
			waitfor_gone(Ident)
	end.

get_aliases() ->
	[
		{"serverrank", ["getserverrank"]}
	].

get_commands() ->
	[
		{"pm", fun pm/1, server},
		{"msg", fun pm/1, server},
		{"age", fun age/1, server},
		{"notes", fun notes/1, server},
		{"notify", fun notify/1, user},
		{"setserverrank", fun serverrank/1, host},
		{"getserverrank", fun gsr/1, server},
		{"info", fun info/1, server}
	].

initialise() ->
	case config:get_value(config, [?MODULE]) of
		{Server, Port, Pwd} -> spawn(server, sloop, [Server, Port, 45678, Pwd]);
		_ -> logging:log(error, "SERVER", "Loaded with no or incorrect config!")
	end.

deinitialise() ->
	case whereis(server) of
		undefined -> ok;
		Pid -> Pid ! stop
	end,
	waitfor_gone(server).

%

gsr(#{reply:=RT, ping:=P, params:=[]}) ->
	case file:consult("ranks.crl") of
		{ok, [Dict]} -> {irc, {msg, {RT, [P, string:join(lists:map(fun({A,_})-> [hd(A),160,tl(A)] end, Dict), "; ")]}}};
		_ -> {irc, {msg, {RT, [P, "Failed to read file!"]}}}
	end;
gsr(#{reply:=RT, ping:=P, params:=List}) ->
	case file:consult("ranks.crl") of
		{ok, [Dict]} ->
			{irc, {msg, {RT, [P, string:join(lists:map(fun(A)-> grank(string:to_lower(A),Dict) end, List), "; ")]}}};
		_ -> {irc, {msg, {RT, [P, "Failed to read file!"]}}}
	end.
grank(N, Dict) ->
	case lists:keyfind(N, 1, Dict) of
		{_, Rank} -> io_lib:format("~s\xa0~s: ~s", [[hd(N)], tl(N), Rank]);
		_ -> io_lib:format("~s\xa0~s unknown", [[hd(N)], tl(N)])
	end.


serverrank(#{reply:=RT, ping:=Ping, params:=[Rank | RWho]}) when RWho /= [] ->
	case file:consult("ranks.crl") of
		{ok, [Dict]} ->
			Who = string:to_lower(string:join(RWho, " ")),
			case lists:keyfind(Who, 1, Dict) of
				{_, CRank} -> Message = ["Set rank of ", Who, " to ", Rank, " - was ", CRank];
				_ ->          Message = ["Set rank of ", Who, " to ", Rank]
			end,
			if
				Rank == "none" -> NewDict = lists:keydelete(Who, 1, Dict);
				true -> NewDict = lists:keystore(Who, 1, Dict, {Who, Rank})
			end,
			file:write_file("ranks.crl", io_lib:format("~p.~n", [NewDict])),
			{irc, {msg, {RT, [Ping, Message]}}};
		error ->
			{irc, {msg, {RT, [Ping, "Failed to read file!"]}}}
	end;
serverrank(#{reply:=RT, ping:=Ping}) -> {irc, {msg, {RT, [Ping, "Provide a rank and a nick; e.g. 'setserverrank Admin CoolGuy3000'."]}}}.


pm(#{reply:=RT, ping:=Ping, params:=[]}) -> {irc, {msg, {RT, [Ping, "Provide a user to PM and a message."]}}};
pm(#{reply:=RT, ping:=Ping, params:=[_]}) -> {irc, {msg, {RT, [Ping, "Provide a message."]}}};
pm(#{nick:=N, reply:=RT, ping:=Ping, params:=Params}) ->
	server ! {pm, N, RT, Ping, string:to_lower(hd(Params)), string:join(tl(Params), " ")},
	ok.

notes(#{reply:=RT, ping:=Ping, params:=[]}) -> {irc, {msg, {RT, [Ping, "Provide a user to find notes for."]}}};
notes(#{nick:=N, reply:=RT, ping:=Ping, params:=[Key]}) ->
	case RT of
		N -> server ! {notes, N, RT, Ping, string:to_lower(Key)};
		_ -> case lists:member(server, bot:rankof_chan(RT)) of
			true -> server ! {notes, N, RT, Ping, string:to_lower(Key)};
			false -> server ! {notes, N, N, "", string:to_lower(Key)}
		end
	end,
	ok;
notes(#{reply:=RT, ping:=Ping}) -> {irc, {msg, {RT, [Ping, "Provide a single key."]}}}.

age(#{reply:=RT, ping:=Ping, params:=[]}) -> {irc, {msg, {RT, [Ping, "Provide a key to check the age of."]}}};
age(#{reply:=RT, ping:=Ping, params:=[Key]}) ->
	server ! {age, RT, Ping, Key},
	ok;
age(#{reply:=RT, ping:=Ping}) -> {irc, {msg, {RT, [Ping, "Provide a single key."]}}}.

notify(#{nick:=N, reply:=RT, ping:=Ping}) ->
	server ! {notify, N, RT, Ping},
	ok.

info(#{reply:=RT, ping:=P, params:=[]}) -> {irc, {msg, {RT, [P, "Provide something to find info on!"]}}};
info(#{reply:=RT, ping:=P, params:=Params}) ->
	server ! {info, RT, P, string:join(Params, " ")},
	ok.

sloop(Svr, Prt, SPrt, Pwd) ->
	case gen_tcp:listen(SPrt, [list, {packet, http}, {active, false}, {reuseaddr, true}]) of
		{ok, SvrSock} ->
			logging:log(info, "SERVER", "Starting loop."),
			register(server, self()),
			loop(SvrSock, Svr, Prt, SPrt, Pwd, sets:new()),
			logging:log(info, "SERVER", "Ending loop."),
			gen_tcp:close(SvrSock);
		{error, Reason} ->
			logging:log(error, "SERVER", "Failed to open listen socket: ~p", [Reason])
	end.

loop(SvrSock, Svr, Prt, SPrt, Pwd, Notify) ->
	case receive
		{pm, Sender, ReplyChannel, ReplyPing, Recipient, Message} ->
			logging:log(info, "SERVER", "sending PM ~s to ~s", [Message, Recipient]),
			case file:consult("ranks.crl") of
				{ok, [RDict]} ->
					case lists:keyfind(string:to_lower(Sender), 1, RDict) of
						{_, Rank} -> ok;
						_ ->
							common:debug("debug", "dict is ~p without key ~p", [RDict, Sender]),
							Rank = "Unknown"
					end;
				_ -> Rank = "Admin"
			end,
			Reply = case byond:send(Svr, Prt, io_lib:format("?adminmsg=~s;msg=~s;key=~s;sender=~s;rank=~s", lists:map(fun byond:vencode/1, [Recipient, Message, Pwd, Sender, Rank]))) of
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
			logging:log(info, "SERVER", "finding notes for ~s", [Lookup]),
			Reply = case byond:send(Svr, Prt, io_lib:format("?notes=~s;key=~s", lists:map(fun byond:vencode/1, [Lookup, Pwd]))) of
				{error, T} -> io_lib:format("Error: ~s", [T]);
				Dict ->
					case hd(orddict:fetch_keys(Dict)) of
						"No information found on the given key." -> ["No information found on the key '", Lookup, "'."];
						T ->
							File = re:replace(base64:encode(erlang:md5(T)), "/", "@", [global]),
							Filename = io_lib:format("/home/bot32/www/~s.txt", [File]),
							file:write_file(Filename, T),
							["Following link valid for approximately ten minutes: http://nyx.gn32.uk/admin/", File, ".txt"]
					end
			end,
			core ! {irc, {msg, {ReplyChannel, [ReplyPing, Reply]}}},
			ok;
		{age, ReplyChannel, ReplyPing, Key} ->
			logging:log(info, "SERVER", "finding age for ~s", [Key]),
			Reply = case byond:send(Svr, Prt, io_lib:format("?age=~s;key=~s", lists:map(fun byond:vencode/1, [Key, Pwd]))) of
				{error, T} -> io_lib:format("Error: ~s", [T]);
				Dict -> ["Age of ", Key, ": ", hd(orddict:fetch_keys(Dict))]
			end,
			core ! {irc, {msg, {ReplyChannel, [ReplyPing, Reply]}}},
			ok;
		{info, ReplyChannel, ReplyPing, Search} ->
			logging:log(info, "SERVER", "finding info for ~s", [Search]),
			Reply = case byond:send(Svr, Prt, io_lib:format("?info=~s;key=~s", lists:map(fun byond:vencode/1, [Search, Pwd]))) of
				{error, T} -> io_lib:format("Error: ~s", [T]);
				[{"No matches",_}] -> "No matches found";
				Dict ->
					case orddict:find("key", Dict) of
						{ok,_} -> % specific player
							D = fun(T) ->
								case orddict:find(T, Dict) of
									{ok, V} -> V;
									error -> "???"
								end
							end,
							Damage = case D("damage") of
									"non-living" -> "non-living";
									Dmg -> string:join(lists:map(fun({A,B}) -> [A,": ",B] end, byond:params2dict(Dmg)), ", ")
							end,
							[
								D("key"), "/(", D("name"), ") ", D("role"), $/, D("antag"), case D("hasbeenrev") of "1" -> " (has been rev)"; _ -> "" end,
								"; loc:\xa0", D("loc"), "; turf:\xa0", D("turf"), "; area:\xa0", lists:filter(fun(T) -> 32 =< T andalso T =< 127 end, D("area")),
								"; stat:\xa0", D("stat"), ", damage:\xa0[", Damage, $],
								"; type:\xa0", D("type"), "; gender:\xa0", D("gender")
							];
						error -> % key->name
							string:join(lists:map(fun({Key,Name}) -> io_lib:format("~s/(~s)", [Key,Name]) end, Dict), "; ")
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
			logging:log(info, "SERVER", "setting notify as ~p", [sets:to_list(N)]),
			loop(SvrSock, Svr, Prt, SPrt, Pwd, N);
		stop -> ok;
		E -> logging:log(error, "SERVER", "received unknown status ~p, exiting", [E])
	end.

readsock(Socket, Pwd, Notify) ->
	% timeout specified just to avoid crashing the bot if bad things happen
	case gen_tcp:recv(Socket, 0, 20000) of
		{ok, {http_request, 'GET', URL, _}} ->
			Plist = tl(URL), % a=x;b=y;c=z
			Dict = byond:params2dict(Plist),
			case orddict:find("pwd", Dict) of
				{ok, Pwd} ->
					case {orddict:find("chan", Dict), orddict:find("mesg", Dict)} of
						{{ok, Chan}, {ok, Mesg}} ->
							Msg = re:replace(Mesg, "[\r\n\t]+", " ", [global, {return, list}]),
							logging:log(info, "SERVER", "relaying to ~s: ~s", [Chan, Msg]),
							case string:str(Msg, "Server starting up on ") of
								1 ->
									lists:foreach(fun(T) -> core ! {irc, {msg, {T, Msg}}} end, sets:to_list(Notify)),
									self() ! {notify, sets:new()};
								_ -> ok
							end,
							case string:str(Msg, "A round of ") of
								1 -> lists:foreach(fun(T) -> core ! {irc, {msg, {T, Msg}}} end, sets:to_list(Notify));
								_ -> ok
							end,
							sendmsg(Chan, Msg);
						_ -> logging:log(info, "SERVER", "received correctly keyed message with no channel and/or message")
					end;
				{ok, NotPwd} -> logging:log(info, "SERVER", "received incorrect key ~s", [NotPwd]);
				error -> logging:log(info, "SERVER", "received non-keyed message '~s'", [Plist])
			end,
			gen_tcp:send(Socket, "HTTP/1.1 200 OK\r\n\r\n"),
			gen_tcp:close(Socket);
		{error, T} -> logging:log(error, "SERVER", "error in readsock: ~p", [T])
	end.

sendmsg(Chan, Msg) when length(Msg) > 350 ->
	{A,B} = lists:split(350, Msg),
	core ! {irc, {msg, {Chan, A}}},
	sendmsg(Chan, "<continued> " ++ B);
sendmsg(Chan, Msg) ->
	core ! {irc, {msg, {Chan, Msg}}}.
