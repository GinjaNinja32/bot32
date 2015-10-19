-module(admin).
-compile(export_all).

-include("definitions.hrl").

get_commands() ->
	[
		{"setrank", fun setrank/4, host},
		{"getrank", fun getrank/4, admin},
		{"whorank", fun whorank/4, admin},
		{"editrank", fun editrank/4, host},
		{"getchanperm", fun gchanperm/4, admin},
		{"chanperm", fun chanperm/4, host},
		{"join", fun join/4, admin},
		{"part", fun part/4, admin},
		{"nick", fun nick/4, admin},
		{"quit", fun quit/4, host},
		{"speak", fun speak/4, admin},
		{"notice", fun notice/4, admin},
		{"action", fun action/4, admin},
		{"prefix", fun prefix/4, host},
		{"cmode", fun mode/4, admin},
		{"kick", fun kick/4, admin},
		{"raw", fun raw/4, host}
	].

tuplefor(N,U,H) -> {list_to_binary(string:to_lower(N)), list_to_binary(U), list_to_binary(H)}.

cleandict(Dict) ->
	orddict:filter(fun(_, V) -> V /= [user] end, Dict).

chanperm(_, ReplyTo, Ping, []) -> {irc, {msg, {ReplyTo, [Ping, "Provide a rank to set and one or more channels"]}}};
chanperm(_, ReplyTo, Ping, [_]) -> {irc, {msg, {ReplyTo, [Ping, "Provide one or more channels"]}}};
chanperm(_, ReplyTo, Ping, [Rank | Chans]) ->
	NewDict = lists:foldl(fun(Chan, Dict) ->
		CRank = bot:rankof_chan(Chan, Dict),
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

gchanperm(_, ReplyTo, Ping, [Channel]) -> {irc, {msg, {ReplyTo, [Ping, Channel, io_lib:format(" has the default permissions: ~p", [bot:rankof_chan(Channel)])]}}};
gchanperm(_, ReplyTo, Ping, _) -> {irc, {msg, {ReplyTo, [Ping, "Provide exactly one channel!"]}}}.

editrank(_, ReplyTo, Ping, []) -> {irc, {msg, {ReplyTo, [Ping, "Provide a rank to edit and one or more nicks (+ users/masks)"]}}};
editrank(_, ReplyTo, Ping, [_]) -> {irc, {msg, {ReplyTo, [Ping, "Provide one or more nicks (+ users/masks)"]}}};
editrank(_, ReplyTo, Ping, [Rank | Masks]) ->
	NewDict = lists:foldl(fun(Mask, Dict) ->
		case re:run(Mask, "^([^!@]+)!([^!@]+)@([^!@]+)$", [{capture, all_but_first, list}]) of
			nomatch ->
				core ! {irc, {msg, {ReplyTo, [Ping, "Failed to parse ", Mask, " as a Nick!User@Host string!"]}}},
				Dict;
			{match,[N,U,H]} ->
				Usr = #user{nick=string:to_lower(N), username=U, host=H},
				CRank = bot:rankof(Usr, Dict),
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

setrank(_, ReplyTo, Ping, []) -> {irc, {msg, {ReplyTo, [Ping, "Please provide a rank to grant and one or more nicks!"]}}};
setrank(_, ReplyTo, Ping, [Rank]) -> {irc, {msg, {ReplyTo, [Ping, "Please provide one or more nicks to grant ", Rank, " to!"]}}};
setrank(_, ReplyTo, Ping, [Rank | Nicks]) ->
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
								CRank = bot:rankof(Usr, Dict),
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

getrank(_, ReplyTo, Ping, []) -> {irc, {msg, {ReplyTo, [Ping, "Please provide a user to check rank for!"]}}};
getrank(_, ReplyTo, Ping, [Mask]) ->
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
			R = bot:rankof(#user{nick=N,username=U,host=H}),
			io_lib:format("Rank of ~s!~s@~s: ~w", [N,U,H,R])
	end,
	{irc, {msg, {ReplyTo, [Ping, Reply]}}}.

whorank(_, ReplyTo, Ping, _) ->
	Reply = util:binary_join(lists:map(fun({N,_,_})->N; (C) -> <<"CH:", C/binary>> end, orddict:fetch_keys(config:require_value(config, [permissions]))), <<",">>),
	{irc, {msg, {ReplyTo, [Ping, Reply]}}}.

join(_, ReplyTo, Ping, []) ->
	{irc, {msg, {ReplyTo, [Ping, "Please provide a channel to join."]}}};
join(_, ReplyTo, Ping, Params) ->
	{multi, [
		{irc, {msg, {ReplyTo, [Ping, "Joining ", hd(Params), "."]}}},
		{irc, {join, hd(Params)}}
	]}.

part(_, ReplyTo, Ping, []) ->
	{irc, {msg, {ReplyTo, [Ping, "Please provide a channel to part."]}}};
part(_, ReplyTo, Ping, Params) ->
	{multi, [
		{irc, {msg, {ReplyTo, [Ping, "Parting ", hd(Params), "."]}}},
		{irc, {part, {hd(Params), string:join(tl(Params), " ")}}}
	]}.

nick(_, RT, Ping, []) -> {irc, {msg, {RT, [Ping, "Please provide a nick for me."]}}};
nick(_, _, _, Params) -> {irc, {nick, hd(Params)}}.

quit(_, _, _, Params) -> {irc, {quit, string:join(Params, " ")}}.

speak (_, RT, Ping, []) -> {irc, {msg, {RT, [Ping, "Please provide a channel and message."]}}};
speak (_, RT, Ping, [_]) -> {irc, {msg, {RT, [Ping, "Please provide a message."]}}};
speak (_, _, _, Params) -> {irc, {msg,          {hd(Params), string:join(tl(Params), " ")}}}.

notice(_, RT, Ping, [_]) -> {irc, {msg, {RT, [Ping, "Please provide a message."]}}};
notice(_, RT, Ping, []) -> {irc, {msg, {RT, [Ping, "Please provide a channel and message."]}}};
notice(_, _, _, Params) -> {irc, {notice,       {hd(Params), string:join(tl(Params), " ")}}}.

action(_, RT, Ping, [_]) -> {irc, {msg, {RT, [Ping, "Please provide an action."]}}};
action(_, RT, Ping, []) -> {irc, {msg, {RT, [Ping, "Please provide a channel and action."]}}};
action(_, _, _, Params) -> {irc, {ctcp, {action, hd(Params), string:join(tl(Params), " ")}}}.


prefix(_, ReplyTo, Ping, []) -> {irc, {msg, {ReplyTo, [Ping, "Please provide a prefix to use for commands."]}}};
prefix(_, ReplyTo, Ping, Params) ->
	Prefix = hd(Params),
	config:set_value(config, [bot, prefix], Prefix),
	{irc, {msg, {ReplyTo, [Ping, "Prefix set to ", Prefix]}}}.

mode(ReplyTo, ReplyTo, Ping, _) -> {irc, {msg, {ReplyTo, [Ping, "Use this in a channel, not query."]}}};
mode(_, ReplyTo, _, Params) -> {irc, {mode, {ReplyTo, string:join(Params, " ")}}}.

kick(_, Chan, _, [User | Reason]) ->
	core ! {irc, {kick, {Chan, User, Reason}}},
	ok;
kick(_, RT, P, _) ->
	{irc, {msg, {RT, [P, "Provide a user to kick and an optional reason"]}}}.

raw(_, _, _, Params) ->
	core ! {raw, string:join(Params, " ")},
	ok.
