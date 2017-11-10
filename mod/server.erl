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
		{"laws",     generic(laws,    "server"), server},
		{"pm",       generic(pm,      "server"), server},
		{"asearch",  generic(asearch, "server"), server}
	].

initialise() ->
	spawn(?MODULE, sloop, []),
	inets:start(),
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

do_add_remove(Nick, OldRank, NewRank) ->
	case OldRank of
		"none" -> ok;
		_ ->
			case config:get_value(config, [?MODULE, chanserv, OldRank]) of
				'$none' -> ok;
				OldChanRanks ->
					lists:foreach(fun({Chan,_}) ->
							core ! {raw, io_lib:format("chanserv access ~s del ~s", [Chan, Nick])}
						end, OldChanRanks)
			end
	end,

	case NewRank of
		"none" -> ok;
		_ ->
			case config:get_value(config, [?MODULE, chanserv, NewRank]) of
				'$none' -> none;
				NewChanRanks ->
					lists:foreach(fun({Chan,Rank}) ->
							core ! {raw, io_lib:format("chanserv access ~s add ~s ~b", [Chan, Nick, Rank])}
						end, NewChanRanks),
					ok
			end
	end.

set_chanserv_rank(Nick, OldRank, Rank, Reply, Ping) ->
	BotNick = config:require_value(config, [bot,nick]),
	core ! {irc, {msg, {"NickServ", ["INFO ",Nick]}}},
	receive
		{irc, {notice, {#user{nick="NickServ",username="services",host="services.sorcery.net"}, BotNick, [_Nick, "is"| _Realname]}}} -> % nick is registered
			case do_add_remove(Nick, OldRank, Rank) of
				none -> core ! {irc, {msg, {Reply, [Ping, "'", Rank, "' did not have an associated ChanServ rank."]}}};
				ok -> core ! {irc, {msg, {Reply, [Ping, Nick, "'s ChanServ entries updated."]}}}
			end;
		{irc, {notice, {#user{nick="NickServ",username="services",host="services.sorcery.net"}, BotNick, ["Nick", _Nick, "isn't", "registered."]}}} -> % not registered
			core ! {irc, {msg, {Reply, [Ping, Nick, "'s nickname is *not* registered with NickServ; they will *not* be added to the ChanServ access lists!"]}}}
	end.

ssr(VID, ID, _, Reply, Ping, [Nick|RankT]) when RankT /= [] ->
	Rank = string:join(RankT, " "),
	OldRank = config:get_value(config, [?MODULE, ranks, ID, string:to_lower(Nick)], "none"),
	ID == "main" andalso set_chanserv_rank(Nick, OldRank, Rank, Reply, Ping),
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
		{error, T} -> io_lib:format("Error: ~p", [T]);
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
					File = io_lib:format("~32.16.0b", [binary:decode_unsigned(crypto:hash(md5, T))]),
					Filename = io_lib:format("/home/bot32/www/~s.txt", [File]),
					file:write_file(Filename, T),
					["Following link valid for approximately ten minutes: http://thanatos.gn32.uk/admin/", File, ".txt"]
			end
	end,
	{irc, {msg, {TrueReply, [TruePing, VID, RMsg]}}}.

age(VID, ID, _, Reply, Ping, [Who|_]) ->
	RMsg = case send2server(ID, "?age=~s;key=~s", [
					Who,
					config:get_value(config, [?MODULE, servers, ID, pass])
				]) of
		{error, T} -> io_lib:format("Error: ~p", [T]);
		Dict -> ["Age of ", Who, ": ", hd(orddict:fetch_keys(Dict))]
	end,
	{irc, {msg, {Reply, [Ping, VID, RMsg]}}}.

info(VID, ID, _, Reply, Ping, What) ->
	RMsg = case send2server(ID, "?info=~s;key=~s", [
					string:join(What, " "),
					config:get_value(config, [?MODULE, servers, ID, pass])
				]) of
		{error, T} -> io_lib:format("Error: ~p", [T]);
		[{"No matches",_}] -> "No matches found";
		Dict ->
			case orddict:find("key", Dict) of
				{ok,_} -> % specific player
					D = fun(T) ->
						case orddict:find(T, Dict) of
							{ok, none} -> "null";
							{ok, V} -> V;
							error -> "???"
						end
					end,
					Damage = case D("damage") of
						"non-living" -> "non-living";
						Dmg -> string:join(lists:map(fun({A,none}) -> [A,": null"]; ({A,B}) -> [A,": ",B] end, byond:params2dict(Dmg)), ", ")
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

laws(VID, ID, _, Reply, Ping, What) ->
	RMsg = case send2server(ID, "?laws=~s;key=~s", [
					string:join(What, " "),
					config:get_value(config, [?MODULE, servers, ID, pass])
				]) of
		{error, T} -> io_lib:format("Error: ~p", [T]);
		[{"No matches",_}] -> "No matches found";
		Dict ->
			case orddict:find("key", Dict) of
				{ok, _} -> % specific player
					D = fun(T) ->
						case orddict:find(T, Dict) of
							{ok, V} -> V;
							error -> "???"
						end
					end,
					ShowLaws = fun(Which, DispStr) ->
						case D(Which) of
							"?" -> ok;
							Params ->
								List = byond:params2dict(Params),
								lists:foreach(fun({T,_}) ->
										core ! {irc, {msg, {Reply, [Ping, VID, DispStr, T]}}}
									end, List)
						end
					end,
					core ! {irc, {msg, {Reply, [Ping, VID, D("name"), $/, D("key"), "'s laws:"]}}},
					ShowLaws("ion", "Ion:  "),
					case D("zero") of
						"?" -> ok;
						Zeroth ->
							core ! {irc, {msg, {Reply, [Ping, VID, "Zero: ", Zeroth]}}}
					end,
					ShowLaws("inherent", "Inh:  "),
					ShowLaws("supplied", "Supp: "),
					ok;

				error -> % key -> name
					string:join(lists:map(fun({Key,Name}) -> io_lib:format("~s/(~s)", [Key,Name]) end, Dict), "; ")
		end
	end,
	case RMsg of
		ok -> ok;
		_ -> {irc, {msg, {Reply, [Ping, VID, RMsg]}}}
	end.


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
	case gen_tcp:listen(45678, [list, {packet, http}, {packet_size, 65536}, {active, false}, {reuseaddr, true}]) of
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
		stop ->
			logging:log(info, ?MODULE, "loop received 'stop'..."),
			stop
	after
		100 ->
			case gen_tcp:accept(SvrSock, 100) of
				{ok, Socket} -> readsock(Socket);
				{error, timeout} -> ok;
				{error, X} ->
					logging:log(info, ?MODULE, "accept() returned {error,~p}", [X]),
					{error, X}
			end
	end of
		ok -> loop(SvrSock);
		stop ->
			logging:log(info, ?MODULE, "loop() received 'stop', returning"),
			ok;
		{error, T} -> logging:log(error, ?MODULE, "received error ~p, exiting", [T])
	end.

dget(Key, Dict) ->
	case orddict:find(Key, Dict) of
		{ok, none} -> {ok, "*null*"};
		X -> X
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
			Response = case case {dget("pwd", Dict), dget("type", Dict)} of
				{{ok, Pwd}, {ok, Type}} ->
					case Type of
						"adminpm" ->
							case lists:map(fun(X) -> dget(X, Dict) end, ["src_key", "src_char", "trg_key", "trg_char", "chan", "msg"]) of
								[{ok,SKey}, {ok,SChar}, {ok,TKey}, {ok,TChar}, {ok,Chan}, {ok,Msg}] ->
									case check_password_for_channel(Pwd, Chan) of
										{ok, ID} -> handle_adminpm(ID, SKey, SChar, TKey, TChar, Chan, Msg), ok;
										error -> forbidden
									end;
								_ -> bad_request
							end;
						"adminhelp" ->
							case lists:map(fun(X) -> dget(X, Dict) end, ["src_key", "src_char", "chan", "msg"]) of
								[{ok,SKey}, {ok,SChar}, {ok,Chan}, {ok,Msg}] ->
									case check_password_for_channel(Pwd, Chan) of
										{ok, ID} -> handle_adminhelp(ID, SKey, SChar, Chan, Msg), ok;
										error -> forbidden
									end;
								_ -> bad_request
							end;
						"ircpm" ->
							case lists:map(fun(X) -> dget(X, Dict) end, ["src_key", "src_char", "rank", "target", "chan", "msg"]) of
								[{ok,SKey}, {ok,SChar}, {ok,Rank}, {ok,Target}, {ok,Chan}, {ok,Msg}] ->
									case check_password_for_channel(Pwd, Chan) of
										{ok, ID} -> handle_ircpm(ID, SKey, SChar, Rank, Target, Chan, Msg), ok;
										error -> forbidden
									end;
								_ -> bad_request
							end;
						"msg" ->
							case {dget("chan", Dict), dget("mesg", Dict)} of
								{{ok, Chan}, {ok, Mesg}} ->
									case check_password_for_channel(Pwd, Chan) of
										{ok, ID} -> handle_msg(ID, Chan, Mesg), ok;
										error -> forbidden
									end;
								_ -> bad_request
							end;
						"runtime" ->
							case {dget("runtimes", Dict), dget("revision", Dict)} of
								{{ok, R}, {ok, Revision}} ->
									RuntimeList = lists:map(fun byond:params2dict/1, byond:params2list(R)),
									case check_password_for_repo(Pwd) of
										{ok, Repo} ->
											runtime_report:report_runtimes(Repo, Revision, RuntimeList),
											ok;
										error -> forbidden
									end;
								_ -> bad_request
							end;
						_ -> bad_request
					end;
				{_, {ok,_}} -> unauthorised;
				_ -> bad_request
			end of
				bad_request -> "400 Bad Request";
				unauthorised -> "401 Unauthorised";
				forbidden -> "403 Forbidden";
				ok -> "204 No Content";
				_ -> "500 Internal Server Error"
			end,
			if
				Response /= "204 No Content" ->
					logging:log(info, ?MODULE, "received invalid/error-causing message ~p", [Plist]),
					logging:log(info, ?MODULE, "response was ~s", [Response]);
				true -> ok
			end,
			gen_tcp:send(Socket, ["HTTP/1.1 ", Response, "\r\n\r\n"]),
			gen_tcp:close(Socket);
		{ok, X} -> logging:log(info, ?MODULE, "received non-GET recv ~p", [X]);
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

check_password_for_channel(Pwd, Chan) -> check_password_generic(Pwd, channels, Chan).
check_password_for_repo(Pwd) ->
	case get_id_from_pass(Pwd) of
		error -> error;
		ID ->
			case config:get_value(config, [?MODULE, servers, ID, repo]) of
				'$none' -> error;
				T -> {ok, T}
			end
	end.
check_password_generic(Pwd, Type, Value) ->
	case get_id_from_pass(Pwd) of
		error -> error;
		ID ->
			case lists:member(Value, config:require_value(config, [?MODULE, servers, ID, Type])) of
				true -> {ok, ID};
				false -> error
			end
	end.

key_name(Key, Char) ->
	[hd(Key), binary_to_list(<<16#feff/utf8>>), tl(Key), $/, $(, Char, $)].

handle_adminpm(ID, SK,SC, TK,TC, Chan, Msg) ->
	Pre = ["PM ", key_name(SK, SC), "->", key_name(TK, TC), ": "],
	handle_admin_message(ID, Pre, Chan, Msg).

handle_adminhelp(ID, SK,SC, Chan, Msg) ->
	Pre = ["HELP from ", key_name(SK,SC), ": "],
	handle_admin_message(ID, Pre, Chan, Msg).

handle_ircpm(ID, SK,SC, Rank, Target, Chan, Msg) ->
	Pre = [Rank, " PM to ", Target, " from ", key_name(SK,SC), ": "],
	handle_admin_message(ID, Pre, Chan, Msg).

handle_admin_message(ID, Pre, Chan, Mesg) ->
	Msg = re:replace(Mesg, "[\r\n\t]+", " ", [global, {return, list}]),
	ahelp_record(ID, [Pre, Msg]),
	sendmsg(Chan, lists:filter(fun(T)->T/=none end, lists:flatten([Pre,Msg]))).

handle_msg(ID, Chan, Mesg) ->
	Msg = re:replace(Mesg, "[\r\n\t]+", " ", [global, {return, list}]),
	case Msg of
		"Server starting up on " ++ _ ->
			lists:foreach(fun(T) -> core ! {irc, {msg, {T, [ID, $:, $ , Msg]}}} end, config:get_value(temp, [?MODULE, notify, ID], [])),
			config:set_value(temp, [?MODULE, notify, ID], []);
		"A round of " ++ _ ->
			lists:foreach(fun(T) -> core ! {irc, {msg, {T, [ID, $:, $ , Msg]}}} end, config:get_value(temp, [?MODULE, notify, ID], []));
		"Reply: " ++ _ -> ahelp_record(ID, Msg);
		"Request for Help " ++ _ -> ahelp_record(ID, Msg);
		_ -> ok
	end,
	sendmsg(Chan, Msg).

ahelp_record(ID, Msg) ->
	file:write_file(["logs/ahelp/ahelp-", ID, ".txt"], [util:format_datetime(calendar:universal_time()), " ", Msg, "\n"], [append]).

asearch(VID, ID, _, Reply, Ping, Params) ->
	os:putenv("pattern", string:join(Params, " ")),
	os:putenv("serverid", ID),
	Result = os:cmd("bash -c './ahelp_search.sh \"$pattern\" \"$serverid\"'"),
	{irc, {msg, {Reply, [VID, Ping, Result]}}}.
%	{irc, {msg, {Reply, [VID, Ping, "Search complete at http://nyx.gn32.uk/admin/", MD5, ".txt"]}}}.

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
