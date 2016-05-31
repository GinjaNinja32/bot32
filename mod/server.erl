-module(server).
-compile(export_all).

-include("definitions.hrl").

get_commands() ->
	[
		{"notify",   generic(notify,   none),    user},
		{"unnotify", generic(unnotify, none),    user},
		{"ssr",      generic(ssr,     "host"),   server},
		{"gsr",      generic(gsr,     "host"),   server},
		{"age",      generic(age,     "server"), server},
		{"notes",    generic(notes,   "server"), server},
		{"info",     generic(info,    "server"), server},
		{"pm",       generic(pm,      "server"), server}
	].

initialise() ->
	spawn(?MODULE, sloop, []),
	ok.

deinitialise() ->
	Pid = config:get_value(temp, [?MODULE, general]),
	Pid ! stop,
	waitfordeath(Pid).

waitfordeath(Pid) ->
	case is_process_alive(Pid) of
		true ->
			timer:sleep(100),
			waitfordeath(Pid);
		false -> ok
	end.

%

generic(Cmd, ReqPerm) ->
	fun(#{origin:=User, nick:=Nick, reply:=Reply, ping:=Ping, params:=Params, selector:=Selector}) ->
		case if
			Selector /= [] ->
				case config:get_value(config, [?MODULE, servers, string:to_lower(Selector)]) of
					'$none' -> {error, "Invalid server selector!"};
					_ -> {string:to_lower(Selector), [string:to_lower(Selector),$:,$ ]}
				end;
			true ->
				Chan = if
					Nick == Reply -> query;
					true -> Reply
				end,
				case config:get_value(config, [?MODULE, default, Chan]) of
					'$none' ->
						case config:get_value(config, [?MODULE, default, default]) of
							'$none' -> {error, "You must provide a server to use!"};
							T -> {T, ""}
						end;
					T -> {T, ""}
				end
		end of
			{error, Msg} -> {irc, {msg, {Reply, [Ping, Msg]}}};
			{ID, VID} ->
				case ReqPerm == none orelse permissions:hasperm(User, Reply, list_to_atom(lists:flatten([ReqPerm,$_|ID]))) of
					true -> ?MODULE:Cmd(VID, ID, Nick, Reply, Ping, Params);
					false -> {irc, {msg, {Reply, [Ping, "You are not authorised to do that."]}}}
				end
		end
	end.

% Commands

gsr(VID,ID, _, Reply, Ping, []) ->
	{irc, {msg, {Reply, [Ping, VID, string:join(lists:map(fun({A,_}) -> [hd(A),160,tl(A)] end, config:get_value(config, [?MODULE, ranks, ID], [])), "; ")]}}};
gsr(VID,ID, _, Reply, Ping, Params) ->
	{irc, {msg, {Reply, [Ping, VID, string:join(lists:map(fun(A) -> [A, " = ", gsrank(ID, string:to_lower(A))] end, Params), "; ")]}}}.

grank(ID, Who) ->
	config:get_value(config, [?MODULE, ranks, ID, Who]).
gsrank(ID, Who) ->
	case grank(ID, Who) of
		'$none' -> "(none)";
		T -> T
	end.

ssr(VID, ID, _, Reply, Ping, [Nick|RankT]) when RankT /= [] ->
	Rank = string:join(RankT, " "),
	ShowRank = case Rank of
		"none" -> config:del_value(config, [?MODULE, ranks, ID, string:to_lower(Nick)]), "<none>";
		_ -> config:set_value(config, [?MODULE, ranks, ID, string:to_lower(Nick)], Rank), [$", Rank, $"]
	end,
	{irc, {msg, {Reply, [Ping, VID, io_lib:format("Set rank of ~s to ~s.", [Nick, ShowRank])]}}};
ssr(_, _, _, Reply, Ping, _) -> {irc, {msg, {Reply, [Ping, "Provide a nick and a rank to set!"]}}}.

pm(VID, ID, Nick, Reply, Ping, [Who | Msg]) when Msg /= [] ->
	RMsg = case send2server(ID, "?adminmsg=~s;msg=~s;key=~s;sender=~s;rank=~s", [
					Who,
					string:join(Msg, " "),
					config:get_value(config, [?MODULE, servers, ID, pass]),
					Nick,
					config:get_value(config, [?MODULE, ranks, ID, string:to_lower(Nick)], "Unknown")
				]) of
		{error, T} -> io_lib:format("Error: ~s", [T]);
		Dict ->
			case orddict:fetch_keys(Dict) of
				["Message Successful"] -> "Sent.";
				[T] -> T
			end
	end,
	{irc, {msg, {Reply, [Ping, VID, RMsg]}}}.

notes(VID, ID, Nick, Reply, Ping, [Who|_]) ->
	{TrueReply, TruePing} = case lists:member(list_to_atom(lists:flatten(["server_",ID])), permissions:rankof_chan(Reply)) of
		true -> {Reply, Ping};
		false -> {Nick, []}
	end,
	RMsg = case send2server(ID, "?notes=~s;key=~s", [
					Who,
					config:get_value(config, [?MODULE, servers, ID, pass])
				]) of
		{error, T} -> io_lib:format("Error: ~p", [T]);
		Dict ->
			case hd(orddict:fetch_keys(Dict)) of
				"No information found on the given key." -> ["No information found on the key '", Who, "'."];
				T ->
					File = re:replace(base64:encode(erlang:md5(T)), "/", "@", [global]),
					Filename = io_lib:format("/home/bot32/www/~s.txt", [File]),
					file:write_file(Filename, T),
					["Following link valid for approximately ten minutes: http://nyx.gn32.uk/admin/", File, ".txt"]
			end
	end,
	{irc, {msg, {TrueReply, [TruePing, VID, RMsg]}}}.

age(VID, ID, _, Reply, Ping, [Who|_]) ->
	RMsg = case send2server(ID, "?age=~s;key=~s", [
					Who,
					config:get_value(config, [?MODULE, servers, ID, pass])
				]) of
		{error, T} -> io_lib:format("Error: ~s", [T]);
		Dict -> ["Age of ", Who, ": ", hd(orddict:fetch_keys(Dict))]
	end,
	{irc, {msg, {Reply, [Ping, VID, RMsg]}}}.

info(VID, ID, _, Reply, Ping, What) ->
	RMsg = case send2server(ID, "?info=~s;key=~s", [
					string:join(What, " "),
					config:get_value(config, [?MODULE, servers, ID, pass])
				]) of
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
					Space = <<16#a0/utf8>>,
					[
						D("key"), "/(", D("name"), ") ", D("role"), $/, D("antag"), case D("hasbeenrev") of "1" -> " (has been rev)"; _ -> "" end,
						"; loc:", Space, D("loc"), "; turf:", Space, D("turf"), "; area:", Space, lists:filter(fun(T) -> 32 =< T andalso T =< 127 end, D("area")),
						"; stat:", Space, D("stat"), ", damage:", Space, "[", Damage, $],
						"; type:", Space, D("type"), "; species:", Space, D("species"), "; gender:", Space, D("gender")
					];
				error -> % key->name
					string:join(lists:map(fun({Key,Name}) -> io_lib:format("~s/(~s)", [Key,Name]) end, Dict), "; ")
		end
	end,
	{irc, {msg, {Reply, [Ping, VID, RMsg]}}}.


notify(VID, ID, Nick, Reply, Ping, _) ->
	case lists:member(string:to_lower(Nick), config:get_value(temp, [?MODULE, notify, ID], [])) of
		true -> {irc, {msg, {Reply, [Ping, VID, "You are already on the list to be notified!"]}}};
		false ->
			config:mod_get_value(temp, [?MODULE, notify, ID], fun('$none') -> [string:to_lower(Nick)]; (T) -> [string:to_lower(Nick)|T] end),
			{irc, {msg, {Reply, [Ping, VID, "You will be notified when the server restarts."]}}}
	end.

unnotify(VID, ID, Nick, Reply, Ping, _) ->
	case lists:member(string:to_lower(Nick), config:get_value(temp, [?MODULE, notify, ID], [])) of
		false -> {irc, {msg, {Reply, [Ping, VID, "You are not on the list to be notified!"]}}};
		true ->
			config:mod_get_value(temp, [?MODULE, notify, ID], fun(T) -> lists:delete(string:to_lower(Nick), T) end),
			{irc, {msg, {Reply, [Ping, VID, "You have been removed from the notify list."]}}}
	end.

% Util

send2server(ID, Msg, FParams) ->
	Addr = config:get_value(config, [?MODULE, servers, ID, address]),
	Port = config:get_value(config, [?MODULE, servers, ID, port]),
	byond:send(Addr, Port, io_lib:format(Msg, lists:map(fun byond:vencode/1, FParams))).

% Receive loop

sloop() ->
	case gen_tcp:listen(45678, [list, {packet, http}, {active, false}, {reuseaddr, true}]) of
		{ok, SvrSock} ->
			config:set_value(temp, [?MODULE, general], self()),
			logging:log(info, ?MODULE, "Starting loop."),
			loop(SvrSock),
			logging:log(info, ?MODULE, "Ending loop."),
			config:del_value(temp, [?MODULE, general]),
			gen_tcp:close(SvrSock);
		{error, Reason} ->
			logging:log(error, ?MODULE, "Failed to open listen socket: ~p", [Reason])
	end.

loop(SvrSock) ->
	case receive
		stop -> stop
	after
		100 ->
			case gen_tcp:accept(SvrSock, 100) of
				{ok, Socket} -> readsock(Socket);
				{error, timeout} -> ok;
				{error, X} -> {error, X}
			end
	end of
		ok -> loop(SvrSock);
		stop -> ok;
		{error, T} -> logging:log(error, ?MODULE, "received error ~p, exiting\n", [T])
	end.

readsock(Socket) ->
	% timeout specified just to avoid crashing the bot if bad things happen
	case gen_tcp:recv(Socket, 0, 60000) of
		{ok, {http_request, 'GET', URL, _}} ->
			Plist = case URL of
				{abs_path, T} -> tl(tl(T));
				T -> tl(T)
			end,
			Dict = byond:params2dict(Plist),
			case {orddict:find("pwd", Dict), orddict:find("chan", Dict), orddict:find("mesg", Dict)} of
				{{ok, Pwd}, {ok, Chan}, {ok, Mesg}} ->
					case check_password_for_channel(Pwd, Chan) of
						{ok, ID} -> handle_msg(ID, Chan, Mesg);
						error -> logging:log(error, ?MODULE, "received invalid message ~p\n", [Plist])
					end;
				_ -> logging:log(info, ?MODULE, "received invalid message ~p\n", [Plist])
			end,
			gen_tcp:send(Socket, "HTTP/1.1 200 OK\r\n\r\n"),
			gen_tcp:close(Socket);
		{error, T} -> logging:log(error, ?MODULE, "error in readsock: ~p", [T])
	end.

get_id_from_pass(Pass) ->
	case lists:filtermap(fun({ID,Dict}) ->
				case orddict:find(pass, Dict) of
					{ok, Pass} -> {true, ID};
					_ -> false
				end
			end, config:get_value(config, [?MODULE, servers])) of
		[ID] -> ID;
		_ -> error
	end.

check_password_for_channel(Pwd, Chan) ->
	case get_id_from_pass(Pwd) of
		error -> error;
		ID ->
			case lists:member(Chan, config:require_value(config, [?MODULE, servers, ID, channels])) of
				true -> {ok, ID};
				false -> error
			end
	end.

handle_msg(ID, Chan, Mesg) ->
	Msg = re:replace(Mesg, "[\r\n\t]+", " ", [global, {return, list}]),
	logging:log(info, ?MODULE, "relaying to ~s: ~s", [Chan, Msg]),
	case string:str(Msg, "Server starting up on ") of
		1 ->
			lists:foreach(fun(T) -> core ! {irc, {msg, {T, [ID, $:, $ , Msg]}}} end, config:get_value(temp, [?MODULE, notify, ID], [])),
			config:set_value(temp, [?MODULE, notify, ID], []);
		_ -> ok
	end,
	case string:str(Msg, "A round of ") of
		1 -> lists:foreach(fun(T) -> core ! {irc, {msg, {T, [ID, $:, $ , Msg]}}} end, config:get_value(temp, [?MODULE, notify, ID], []));
		_ -> ok
	end,
	sendmsg(Chan, Msg).

sendmsg(none, _) -> ok;
sendmsg(Chan, Msg) ->
	sendmsg(Chan, lists:map(fun list_to_binary/1, string:tokens(Msg, " ")), <<>>).

sendmsg(Chan, [], Bin) ->
	core ! {irc, {msg, {Chan, Bin}}};
sendmsg(Chan, [Fst|Rst], Bin) ->
	NewBin = case byte_size(Bin) + byte_size(Fst) of
		T when T > 380 ->
			core ! {irc, {msg, {Chan, Bin}}},
			<<"<continued> ">>;
		_ when Bin == <<>> ->
			<<>>;
		_ ->
			<<Bin/binary, 32>>
	end,
	sendmsg(Chan, Rst, <<NewBin/binary, Fst/binary>>).
