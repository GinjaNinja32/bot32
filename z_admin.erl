-module(z_admin).
-compile(export_all).

-include("definitions.hrl").

get_commands() ->
	[
%		{"admin", fun admin/5, admin},
%		{"deadmin", fun deadmin/5, admin},
		{"setrank", fun setrank/5, host},
		{"getrank", fun getrank/5, admin},
		{"whorank", fun whorank/5, admin},
		{"editrank", fun editrank/5, host},
		{"loadranks", fun loadranks/5, host},
		{"getchanperm", fun gchanperm/5, admin},
		{"chanperm", fun chanperm/5, host},
		{"join", fun join/5, admin},
		{"part", fun part/5, admin},
		{"nick", fun nick/5, admin},
		{"quit", fun quit/5, host},
		{"speak", fun speak/5, admin},
		{"notice", fun notice/5, admin},
		{"action", fun action/5, admin},
		{"prefix", fun prefix/5, host},
		{"addprefix", fun addprefix/5, host},
		{"remprefix", fun remprefix/5, host},
		{"cmode", fun mode/5, admin},
		{"raw", fun raw/5, host}
	].

initialise(T) -> T.
deinitialise(T) -> T.

tuplefor(N,U,H) ->
	{list_to_binary(string:to_lower(N)), list_to_binary(U), list_to_binary(H)}.

loadranks(_, ReplyTo, Ping, _, State) ->
	case file:consult("permissions.crl") of
		{ok, [Perms]} ->
			core ! {irc, {msg, {ReplyTo, [Ping, "Permissions loaded successfully."]}}},
			{state, State#state{permissions=Perms}};
		{ok, _} -> {irc, {msg, {ReplyTo, [Ping, "Incorrect permission file format."]}}};
		{error, T} -> {irc, {msg, {ReplyTo, [Ping, io_lib:format("Unable to read file: ~p", [T])]}}}
	end.

cleandict(Dict) ->
	orddict:filter(fun(_, V) -> V /= [user] end, Dict).	

chanperm(_, ReplyTo, Ping, [], _) -> {irc, {msg, {ReplyTo, [Ping, "Provide a rank to set and one or more channels"]}}};
chanperm(_, ReplyTo, Ping, [_], _) -> {irc, {msg, {ReplyTo, [Ping, "Provide one or more channels"]}}};
chanperm(_, ReplyTo, Ping, [Rank | Chans], State) ->
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
	end, State#state.permissions, Chans),
	NewState = State#state{permissions=cleandict(NewDict)},
	X = file:write_file("permissions.crl", io_lib:format("~p.~n", [NewState#state.permissions])),
	logging:log(info, "BOT", "permissions save: ~p", [X]),
	{state, NewState}.

gchanperm(_, ReplyTo, Ping, [Channel], State) -> {irc, {msg, {ReplyTo, [Ping, Channel, io_lib:format(" has the default permissions: ~p", [bot:rankof_chan(Channel, State#state.permissions)])]}}};
gchanperm(_, ReplyTo, Ping, _, _) -> {irc, {msg, {ReplyTo, [Ping, "Provide exactly one channel!"]}}}.

editrank(_, ReplyTo, Ping, [], _) -> {irc, {msg, {ReplyTo, [Ping, "Provide a rank to edit and one or more nicks (+ users/masks)"]}}};
editrank(_, ReplyTo, Ping, [_], _) -> {irc, {msg, {ReplyTo, [Ping, "Provide one or more nicks (+ users/masks)"]}}};
editrank(_, ReplyTo, Ping, [Rank | Masks], State) ->
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
		end end, State#state.permissions, Masks),
	NewState = State#state{permissions=cleandict(NewDict)},
	X = file:write_file("permissions.crl", io_lib:format("~p.~n", [NewState#state.permissions])),
	logging:log(info, "BOT", "permissions save: ~p", [X]),
	{state, NewState}.	

setrank(_, ReplyTo, Ping, [], _) -> {irc, {msg, {ReplyTo, [Ping, "Please provide a rank to grant and one or more nicks!"]}}};
setrank(_, ReplyTo, Ping, [Rank], _) -> {irc, {msg, {ReplyTo, [Ping, "Please provide one or more nicks to grant ", Rank, " to!"]}}};
setrank(_, ReplyTo, Ping, [Rank | Nicks], State) ->
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
		end end, State#state.permissions, Nicks),
	NewState = State#state{permissions=cleandict(NewDict)},
	X = file:write_file("permissions.crl", io_lib:format("~p.~n", [NewState#state.permissions])),
	logging:log(info, "BOT", "permissions save: ~p", [X]),
	{state, NewState}
	catch
		throw:return -> {irc, {msg, {ReplyTo, [Ping, "Loop cancelled."]}}}
	end.

getrank(_, ReplyTo, Ping, [], _) -> {irc, {msg, {ReplyTo, [Ping, "Please provide a user to check rank for!"]}}};
getrank(_, ReplyTo, Ping, [Mask], State) ->
	Reply = case re:run(Mask, "([^!@]+)(!([^!@]+)@([^!@]+))?", [{capture, all_but_first, list}]) of
		nomatch -> "Invalid mask!";
		{match, [N]} ->
			Matching = orddict:filter(fun({Nick,_,_},_) -> re:run(N, util:regex_star(Nick), [{capture, none}, caseless]) == match; (_,_) -> false end, State#state.permissions),
			if
				length(Matching) == 0 -> "No results.";
				length(Matching) > 4 -> "Too many results.";
				length(Matching) == 1 ->
					[{{Nx,Ux,Hx},R}] = Matching,
					io_lib:format("Rank of ~s!~s@~s: ~w", [Nx,Ux,Hx,R]);
				true ->	string:join(lists:map(fun({{Nx,Ux,Hx},_}) -> [Nx,$!,Ux,$@,Hx] end, Matching), ", ")
			end;
		{match, [N,_,U,H]} ->
			R = bot:rankof(#user{nick=N,username=U,host=H}, State#state.permissions),
			io_lib:format("Rank of ~s!~s@~s: ~w", [N,U,H,R])
	end,
	{irc, {msg, {ReplyTo, [Ping, Reply]}}}.

whorank(_, ReplyTo, Ping, _, State) ->
	Reply = util:binary_join(lists:map(fun({N,_,_})->N; (C) -> <<"CH:", C/binary>> end, orddict:fetch_keys(State#state.permissions)), <<",">>),
	{irc, {msg, {ReplyTo, [Ping, Reply]}}}.

join(_, ReplyTo, Ping, [], _) ->
	{irc, {msg, {ReplyTo, [Ping, "Please provide a channel to join."]}}};
join(_, ReplyTo, Ping, Params, _) ->
	{multi, [
		{irc, {msg, {ReplyTo, [Ping, "Joining ", hd(Params), "."]}}},
		{irc, {join, hd(Params)}}
	]}.

part(_, ReplyTo, Ping, [], _) ->
	{irc, {msg, {ReplyTo, [Ping, "Please provide a channel to part."]}}};
part(_, ReplyTo, Ping, Params, _) ->
	{multi, [
		{irc, {msg, {ReplyTo, [Ping, "Parting ", hd(Params), "."]}}},
		{irc, {part, {hd(Params), string:join(tl(Params), " ")}}}
	]}.

nick(_, RT, Ping, [], _) -> {irc, {msg, {RT, [Ping, "Please provide a nick for me."]}}};
nick(_, _, _, Params, _) -> {irc, {nick, hd(Params)}}.

quit(_, _, _, Params, _) -> {irc, {quit, string:join(Params, " ")}}.

speak (_, RT, Ping, [], _) -> {irc, {msg, {RT, [Ping, "Please provide a channel and message."]}}};
speak (_, RT, Ping, [_], _) -> {irc, {msg, {RT, [Ping, "Please provide a message."]}}};
speak (_, _, _, Params, _) -> {irc, {msg,          {hd(Params), string:join(tl(Params), " ")}}}.

notice(_, RT, Ping, [_], _) -> {irc, {msg, {RT, [Ping, "Please provide a message."]}}};
notice(_, RT, Ping, [], _) -> {irc, {msg, {RT, [Ping, "Please provide a channel and message."]}}};
notice(_, _, _, Params, _) -> {irc, {notice,       {hd(Params), string:join(tl(Params), " ")}}}.

action(_, RT, Ping, [_], _) -> {irc, {msg, {RT, [Ping, "Please provide an action."]}}};
action(_, RT, Ping, [], _) -> {irc, {msg, {RT, [Ping, "Please provide a channel and action."]}}};
action(_, _, _, Params, _) -> {irc, {ctcp, {action, hd(Params), string:join(tl(Params), " ")}}}.


prefix(_, ReplyTo, Ping, [], _) -> {irc, {msg, {ReplyTo, [Ping, "Please provide a prefix to use for commands."]}}};
prefix(_, ReplyTo, Ping, Params, State=#state{}) ->
	<<Prefix/utf8, _/binary>> = list_to_binary(hd(Params)),
	self() ! {state, State#state{prefix=[Prefix]}},
	{irc, {msg, {ReplyTo, [Ping, <<"Prefix set to ", Prefix/utf8>>]}}}.

addprefix(_, ReplyTo, Ping, [], _) -> {irc, {msg, {ReplyTo, [Ping, "Please provide an additional prefix to use for commands."]}}};
addprefix(_, ReplyTo, Ping, Params, State=#state{}) ->
	<<Prefix/utf8, _/binary>> = list_to_binary(hd(Params)),
	Reply = case State#state.prefix of
		Old when is_number(Old) -> New = [Prefix, Old], [<<"Prefix set to ">>, format_prefixes(New)];
		Old when is_list(Old)   -> New = [Prefix | Old], [<<"Prefix set to ">>,format_prefixes(New)];
		_ -> New = [Prefix], io_lib:format(<<"Unknown prefix format ~w">>, [State#state.prefix])
	end,
	self() ! {state, State#state{prefix=New}},
	{irc, {msg, {ReplyTo, [Ping, Reply]}}}.

remprefix(_, ReplyTo, Ping, [], _) -> {irc, {msg, {ReplyTo, [Ping, <<"Please provide a prefix to stop using for commands.">>]}}};
remprefix(_, ReplyTo, Ping, Params, State=#state{}) ->
	<<ToRemove/utf8, _/binary>> = list_to_binary(hd(Params)),
	Reply = case State#state.prefix of
		Old when is_number(Old) ->
			if
				ToRemove == Old -> New = [];
				true -> New = [Old]
			end,
			[<<"Prefix set to ">>, format_prefixes(New)];
		Old when is_list(Old) ->
			New = lists:delete(ToRemove, Old),
			[<<"Prefix set to ">>, format_prefixes(New)];
		Old ->
			New = Old, io_lib:format(<<"Unknown prefix format ~w">>, [Old])
	end,
	self() ! {state, State#state{prefix=New}},
	{irc, {msg, {ReplyTo, [Ping, Reply]}}}.

format_prefixes(List) -> lists:foldr(fun(T,B) -> <<T/utf8, B/binary>> end, <<>>, List).

mode(ReplyTo, ReplyTo, Ping, _, _) -> {irc, {msg, {ReplyTo, [Ping, "Use this in a channel, not query."]}}};
mode(_, ReplyTo, _, Params, _) -> {irc, {mode, {ReplyTo, string:join(Params, " ")}}}.
	
raw(_, _, _, Params, _) ->
	core ! {raw, string:join(Params, " ")},
	ok.
