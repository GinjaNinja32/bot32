-module(admin).
-compile(export_all).

-include("definitions.hrl").

get_commands() ->
	[
		{"setrank", fun setrank/1, perms},
		{"getrank", fun getrank/1, [{"mask",short}], perms},
		{"whorank", fun whorank/1, admin},
		{"editrank", fun editrank/1, perms},
		{"getchanperm", fun gchanperm/1, admin},
		{"chanperm", fun chanperm/1, perms},
		{"nsperm", fun nsperm/1, perms},
		{"getnsperm", fun gnsperm/1, [short], perms},
		{"join", fun join/1, admin},
		{"part", fun part/1, admin},
		{"nick", fun nick/1, admin},
		{"quit", fun quit/1, host},
		{"speak", fun speak/1, admin},
		{"notice", fun notice/1, admin},
		{"action", fun action/1, admin},
		{"prefix", fun prefix/1, host},
		{"cmode", fun mode/1, admin},
		{"kick", fun kick/1, chanban},
		{"kickban", fun kickban/1, chanban},
		{"raw", fun raw/1, host}
	].

tuplefor(N,U,H) -> {list_to_binary(string:to_lower(N)), list_to_binary(U), list_to_binary(H)}.

cleandict(Dict) ->
	orddict:filter(fun(_, V) -> V /= [user] end, Dict).

chanperm(#{reply:=ReplyTo, ping:=Ping, params:=[]}) -> {irc, {msg, {ReplyTo, [Ping, "Provide a rank to set and one or more channels"]}}};
chanperm(#{reply:=ReplyTo, ping:=Ping, params:=[_]}) -> {irc, {msg, {ReplyTo, [Ping, "Provide one or more channels"]}}};
chanperm(#{reply:=ReplyTo, ping:=Ping, params:=[Rank | Chans]}) ->
	NewDict = lists:foldl(fun(Chan, Dict) ->
		CRank = permissions:rankof_chan(Chan),
		NewList = case Rank of
			"+" ++ R -> case lists:member(list_to_atom(R), CRank) of
					true -> CRank;
					false -> [list_to_atom(R) | CRank]
				end;
			"-" ++ R -> lists:delete(list_to_atom(R), CRank);
			"user" -> [user];
			R -> [user, list_to_atom(R)]
		end,
		core ! {irc, {msg, {ReplyTo, [Ping, "Changed the permissions in ",Chan," to ",io_lib:format("~w", [NewList])]}}},
		orddict:store(list_to_binary(Chan), NewList, Dict)
	end, config:require_value(config, [permissions]), Chans),
	config:set_value(config, [permissions], cleandict(NewDict)).

gchanperm(#{reply:=ReplyTo, ping:=Ping, params:=[Channel]}) -> {irc, {msg, {ReplyTo, [Ping, Channel, io_lib:format(" has the default permissions: ~p", [permissions:rankof_chan(Channel)])]}}};
gchanperm(#{reply:=ReplyTo, ping:=Ping}) -> {irc, {msg, {ReplyTo, [Ping, "Provide exactly one channel!"]}}}.

nsperm(#{reply:=Reply, ping:=Ping, params:=[Rank | Nickserv]}) ->
	NewDict = lists:foldl(fun(NS, Dict) ->
		NSRank = permissions:rankof_ns(NS),
		NewList = case Rank of
			"+" ++ R -> case lists:member(list_to_atom(R), NSRank) of
					true -> NSRank;
					false -> [list_to_atom(R) | NSRank]
				end;
			"-" ++ R -> lists:delete(list_to_atom(R), NSRank);
			"user" -> [user];
			R -> [user, list_to_atom(R)]
		end,
		core ! {irc, {msg, {Reply, [Ping, "Changed the permissions of the nickserv account ",NS," to ",io_lib:format("~w", [NewList])]}}},
		orddict:store({nickserv, list_to_binary(NS)}, NewList, Dict)
	end, config:require_value(config, [permissions]), Nickserv),
	config:set_value(config, [permissions], cleandict(NewDict)).

gnsperm(#{reply:=Reply, ping:=Ping, params:=[NS]}) ->
	{irc, {msg, {Reply, [Ping, io_lib:format("The nickserv account ~s has the permissions ~500p", [NS, permissions:rankof_ns(NS)])]}}}.

editrank(#{reply:=ReplyTo, ping:=Ping, params:=[]}) -> {irc, {msg, {ReplyTo, [Ping, "Provide a rank to edit and one or more nicks (+ users/masks)"]}}};
editrank(#{reply:=ReplyTo, ping:=Ping, params:=[_]}) -> {irc, {msg, {ReplyTo, [Ping, "Provide one or more nicks (+ users/masks)"]}}};
editrank(#{reply:=ReplyTo, ping:=Ping, params:=[Rank | Masks]}) ->
	NewDict = lists:foldl(fun(Mask, Dict) ->
		case re:run(Mask, "^([^!@]+)!([^!@]+)@([^!@]+)$", [{capture, all_but_first, list}]) of
			nomatch ->
				core ! {irc, {msg, {ReplyTo, [Ping, "Failed to parse ", Mask, " as a Nick!User@Host string!"]}}},
				Dict;
			{match,[N,U,H]} ->
				Usr = #user{nick=string:to_lower(N), username=U, host=H},
				CRank = permissions:rankof(Usr),
				NewList = case Rank of
					"+" ++ R ->
						case lists:member(list_to_atom(R), CRank) of
							true -> CRank;
							false -> [list_to_atom(R) | CRank]
						end;
					"-" ++ R ->
						lists:delete(list_to_atom(R), CRank);
					"user" -> [user];
					R -> [user, list_to_atom(R)]
				end,
				core ! {irc, {msg, {ReplyTo, [Ping, "Changed the permissions of ",N,$!,U,$@,H," to ",io_lib:format("~w", [NewList]),$.]}}},
				orddict:store(tuplefor(N,U,H), NewList, Dict)
		end end, config:require_value(config, [permissions]), Masks),
	config:set_value(config, [permissions], cleandict(NewDict)).

setrank(#{reply:=ReplyTo, ping:=Ping, params:=[]}) -> {irc, {msg, {ReplyTo, [Ping, "Please provide a rank to grant and one or more nicks!"]}}};
setrank(#{reply:=ReplyTo, ping:=Ping, params:=[Rank]}) -> {irc, {msg, {ReplyTo, [Ping, "Please provide one or more nicks to grant ", Rank, " to!"]}}};
setrank(#{reply:=ReplyTo, ping:=Ping, params:=[Rank | Nicks]}) ->
	try
	NewDict = lists:foldl(fun(Nick, Dict) ->
		timer:sleep(1000), % Avoid spamming the server with WHOIS requests, forcing it to throttle us and kill the loop
		core ! {raw, ["WHOIS ", Nick]},
		receive
			{irc, {numeric, {{rpl,whois_user}, Params}}} ->
				case Params of
					[_,N,U,H|_] ->
						case string:to_lower(N) == string:to_lower(Nick) of
							true ->
								Usr = #user{nick=string:to_lower(N),username=U,host=H},
								CRank = permissions:rankof(Usr),
								NewList = case Rank of
									"+" ++ R ->
										case lists:member(list_to_atom(R), CRank) of
											true -> CRank;
											false -> [list_to_atom(R) | CRank]
										end;
									"-" ++ R -> lists:delete(list_to_atom(R), CRank);
									"user" -> [user];
									R -> [user, list_to_atom(R)]
								end,
								core ! {irc, {msg, {ReplyTo, [Ping, "Changed the permissions of ",N,$!,U,$@,H," to ",io_lib:format("~w",[NewList])]}}},
								orddict:store(tuplefor(N,U,H), NewList, Dict);
							false -> core ! {irc, {msg, {ReplyTo, [Ping, "Received an incorrect or unexpected WHOIS reply to WHOIS ",Nick,"!"]}}}, Dict
						end;
					_ ->
						logging:log(error, "BOT", "badly formatted message was ~p", [string:join(Params, " ")]),
						core ! {irc, {msg, {ReplyTo, [Ping, "Received a badly-formatted RPL_WHOISUSER message to WHOIS ",Nick,"!"]}}},
						Dict
				end;
			{irc, {numeric, {{err,no_such_nick}, _}}} ->
				core ! {irc, {msg, {ReplyTo, [Ping, "No user with nick ",Nick," found!"]}}}, Dict;
			brkloop -> throw(return)
		after 1000 -> throw(return)
		end end, config:require_value(config, [permissions]), Nicks),
	config:set_value(config, [permissions], cleandict(NewDict))
	catch
		throw:return -> {irc, {msg, {ReplyTo, [Ping, "Loop cancelled."]}}}
	end.

getrank(#{reply:=ReplyTo, ping:=Ping, params:=[Mask]}) ->
	Reply = case re:run(Mask, "([^!@]+)(!([^!@]+)@([^!@]+))?", [{capture, all_but_first, list}]) of
		nomatch -> "Invalid mask!";
		{match, [N]} ->
			Matching = orddict:filter(fun({Nick,_,_},_) -> re:run(N, util:regex_star(Nick), [{capture, none}, caseless]) == match; (_,_) -> false end, config:require_value(config, [permissions])),
			if
				length(Matching) == 0 -> "No results.";
				length(Matching) > 4 -> "Too many results.";
				length(Matching) == 1 ->
					[{{Nx,Ux,Hx},R}] = Matching,
					io_lib:format("Rank of ~s!~s@~s: ~w", [Nx,Ux,Hx,R]);
				true ->	string:join(lists:map(fun({{Nx,Ux,Hx},_}) -> [Nx,$!,Ux,$@,Hx] end, Matching), ", ")
			end;
		{match, [N,_,U,H]} ->
			R = permissions:rankof(#user{nick=N,username=U,host=H}),
			io_lib:format("Rank of ~s!~s@~s: ~w", [N,U,H,R])
	end,
	{irc, {msg, {ReplyTo, [Ping, Reply]}}}.

whorank(#{reply:=ReplyTo, ping:=Ping}) ->
	Reply = util:binary_join(lists:map(fun({N,_,_})->N; (C) -> <<"CH:", C/binary>> end, orddict:fetch_keys(config:require_value(config, [permissions]))), <<",">>),
	{irc, {msg, {ReplyTo, [Ping, Reply]}}}.

join(#{reply:=ReplyTo, ping:=Ping, params:=[]}) ->
	{irc, {msg, {ReplyTo, [Ping, "Please provide a channel to join."]}}};
join(#{reply:=ReplyTo, ping:=Ping, params:=Params}) ->
	core ! {irc, {msg, {ReplyTo, [Ping, "Joining ", hd(Params), "."]}}},
	{irc, {join, hd(Params)}}.

part(#{reply:=ReplyTo, ping:=Ping, params:=[]}) ->
	{irc, {msg, {ReplyTo, [Ping, "Please provide a channel to part."]}}};
part(#{reply:=ReplyTo, ping:=Ping, params:=Params}) ->
	core ! {irc, {msg, {ReplyTo, [Ping, "Parting ", hd(Params), "."]}}},
	{irc, {part, {hd(Params), string:join(tl(Params), " ")}}}.

nick(#{reply:=RT, ping:=Ping, params:=[]}) -> {irc, {msg, {RT, [Ping, "Please provide a nick for me."]}}};
nick(#{params:=Params}) -> {irc, {nick, hd(Params)}}.

quit(#{params:=Params}) -> {irc, {quit, string:join(Params, " ")}}.

speak (#{reply:=RT, ping:=Ping, params:=[]}) -> {irc, {msg, {RT, [Ping, "Please provide a channel and message."]}}};
speak (#{reply:=RT, ping:=Ping, params:=[_]}) -> {irc, {msg, {RT, [Ping, "Please provide a message."]}}};
speak (#{params:=Params}) -> {irc, {msg, {hd(Params), string:join(tl(Params), " ")}}}.

notice(#{reply:=RT, ping:=Ping, params:=[_]}) -> {irc, {msg, {RT, [Ping, "Please provide a message."]}}};
notice(#{reply:=RT, ping:=Ping, params:=[]}) -> {irc, {msg, {RT, [Ping, "Please provide a channel and message."]}}};
notice(#{params:=Params}) -> {irc, {notice, {hd(Params), string:join(tl(Params), " ")}}}.

action(#{reply:=RT, ping:=Ping, params:=[_]}) -> {irc, {msg, {RT, [Ping, "Please provide an action."]}}};
action(#{reply:=RT, ping:=Ping, params:=[]}) -> {irc, {msg, {RT, [Ping, "Please provide a channel and action."]}}};
action(#{params:=Params}) -> {irc, {ctcp, {action, hd(Params), string:join(tl(Params), " ")}}}.


prefix(#{reply:=ReplyTo, ping:=Ping, params:=[]}) ->
	Prefix = config:require_value(config, [bot, prefix]),
	{irc, {msg, {ReplyTo, [Ping, "Prefix is: ", Prefix]}}};
prefix(#{reply:=ReplyTo, ping:=Ping, params:=Params}) ->
	Prefix = hd(Params),
	config:set_value(config, [bot, prefix], Prefix),
	{irc, {msg, {ReplyTo, [Ping, "Prefix set to ", Prefix]}}}.

mode(#{nick:=ReplyTo, reply:=ReplyTo, ping:=Ping}) -> {irc, {msg, {ReplyTo, [Ping, "Use this in a channel, not query."]}}};
mode(#{reply:=ReplyTo, params:=Params}) -> {irc, {mode, {ReplyTo, string:join(Params, " ")}}}.

kick(#{reply:=Chan, params:=[User | Reason]}) ->
	core ! {irc, {kick, {Chan, User, string:join(Reason, " ")}}},
	ok;
kick(#{reply:=RT, ping:=P}) ->
	{irc, {msg, {RT, [P, "Provide a user to kick and an optional reason"]}}}.


kickban(#{reply:=Chan, params:=[User | Reason]}) ->
	Whois = util:whois(User),
	case Whois of
		#{cloak := Cloak} when Cloak /= false ->
			core ! {irc, {mode, {Chan, ["+b *!*@", ban_host(Cloak)]}}};
		#{host := Host} ->
			core ! {irc, {mode, {Chan, ["+b *!*@", ban_host(Host)]}}};
		_ ->
			core ! {irc, {msg, {Chan, ["Failed to find a hostmask for ", User, "!"]}}}
	end,
	core ! {irc, {kick, {Chan, User, string:join(Reason, " ")}}},
	ok;
kickban(#{reply:=RT, ping:=P}) ->
	{irc, {msg, {RT, [P, "Provide a user to kick and an optional reason"]}}}.

ban_host(Host) ->
	["*.", string:join(tl(string:tokens(Host, ".")), ".")].

raw(#{params:=Params}) ->
	core ! {raw, string:join(Params, " ")},
	ok.
