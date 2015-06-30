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
		{"join", fun join/5, admin},
		{"part", fun part/5, admin},
		{"nick", fun nick/5, admin},
		{"quit", fun quit/5, host},
		{"speak", fun speak/5, admin},
		{"notice", fun notice/5, admin},
		{"action", fun action/5, admin},
		{"ignore", fun ignore/5, admin},
		{"unignore", fun unignore/5, admin},
		{"whoignore", fun whoignore/5, admin},
		{"prefix", fun prefix/5, host},
		{"addprefix", fun addprefix/5, host},
		{"remprefix", fun remprefix/5, host}
	].

initialise(T) -> T.
deinitialise(T) -> T.

setrank(_, ReplyTo, Ping, [], _) -> {irc, {msg, {ReplyTo, [Ping, "Please provide a rank to grant and one or more nicks!"]}}};
setrank(_, ReplyTo, Ping, [Rank], _) -> {irc, {msg, {ReplyTo, [Ping, "Please provide one or more nicks to grant ", Rank, " to!"]}}};
setrank(_, ReplyTo, Ping, [Rank | Nicks], State) ->
	try
	NewState =  lists:foldl(fun(Nick, Stat) ->
		core ! {raw, ["WHOIS ", Nick]},
		receive
			{irc, {numeric, {{rpl,whois_user}, Params}}} ->
				case Params of
					[_,N,U,H|_] ->
						case string:to_lower(N) == string:to_lower(Nick) of
							true ->
								Usr = #user{nick=string:to_lower(N),username=U,host=H},
								CRank = bot:rankof(Usr, Stat#state.permissions),
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
								core ! {irc, {msg, {ReplyTo, [Ping, "Changed the permissions of ",N,"!",U,"@",H," to ",io_lib:format("~w",[NewList])]}}},
								Stat#state{permissions=orddict:store({string:to_lower(N),U,H}, NewList, Stat#state.permissions)};
							false -> core ! {irc, {msg, {ReplyTo, [Ping, "Received an incorrect or unexpected WHOIS reply to WHOIS ",Nick,"!"]}}}, Stat
						end;
					_ ->
						logging:log(error, "BOT", "badly formatted message was ~p", [string:join(Params, " ")]),
						core ! {irc, {msg, {ReplyTo, [Ping, "Received a badly-formatted RPL_WHOISUSER message to WHOIS ",Nick,"!"]}}},
						Stat
				end;
			{irc, {numeric, {{err,no_such_nick}, _}}} ->
				core ! {irc, {msg, {ReplyTo, [Ping, "No user with nick ",Nick," found!"]}}}, Stat;
			brkloop -> throw(return)
		after 1000 -> throw(return)
		end end, State, Nicks),
	X = file:write_file("permissions.crl", io_lib:format("~p.~n", [NewState#state.permissions])),
	logging:log(info, "BOT", "permissions save: ~p", [X]),
	{state, NewState}
	catch
		throw:return -> {irc, {msg, {ReplyTo, [Ping, "Loop cancelled."]}}}
	end.

getrank(_, ReplyTo, Ping, [], _) -> {irc, {msg, {ReplyTo, [Ping, "Please provide a user to check rank for!"]}}};
getrank(_, ReplyTo, Ping, [Nick], State) ->
	Reply = case orddict:filter(fun({N,_,_},_) -> string:to_lower(Nick) == N end, State#state.permissions) of
		[] -> "No matching user found!";
		[{{N,U,H},R}] -> io_lib:format("Rank of ~s!~s@~s: ~w", [N,U,H,R]);
		T when is_list(T) -> [integer_to_list(length(T)), " entries found (?)"];
		_ -> "Error."
	end,
	{irc, {msg, {ReplyTo, [Ping, Reply]}}}.

whorank(_, ReplyTo, Ping, _, State) ->
	List = orddict:fetch_keys(State#state.permissions),
	{Initial,_,_} = hd(List),
	Reply = lists:foldl(fun({N,_,_},K) -> [N,","|K] end, Initial, tl(List)),
	{irc, {msg, {ReplyTo, [Ping, Reply]}}}.

%admin(_, ReplyTo, Ping, [], _) ->
%	{irc, {msg, {ReplyTo, [Ping, "Please provide an admin to add."]}}};
%admin(_, ReplyTo, Ping, Params, State=#state{admins=Admins}) ->
%	self() ! {state, State#state{admins=sets:add_element(string:to_lower(hd(Params)), Admins)}},
%	{irc, {msg, {ReplyTo, [Ping, "Added ", hd(Params), " to the admins list."]}}}.

%deadmin(_, ReplyTo, Ping, [], _) ->
%	{irc, {msg, {ReplyTo, [Ping, "Please provide an admin to remove."]}}};
%deadmin(_Origin, ReplyTo, Ping, Params, State=#state{admins=Admins}) ->
%	self() ! {state, State#state{admins=sets:del_element(string:to_lower(hd(Params)), Admins)}},
%	{irc, {msg, {ReplyTo, [Ping, "Removed ", hd(Params), " from the admins list."]}}}.

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


ignore(_, ReplyTo, Ping, [], _) -> {irc, {msg, {ReplyTo, [Ping, "Please provide a nick to ignore."]}}};
ignore(_, ReplyTo, Ping, Params, State=#state{ignore=I}) ->
	Nick = string:to_lower(hd(Params)),
	case sets:is_element(Nick, I) of
		true -> {irc, {msg, {ReplyTo, [Ping, "I am already ignoring ", Nick, "."]}}};
		false -> self() ! {state, State#state{ignore=sets:add_element(Nick, I)}},
			{irc, {msg, {ReplyTo, [Ping, "Now ignoring ", Nick, "."]}}}
	end.

unignore(_, ReplyTo, Ping, [], _) -> {irc, {msg, {ReplyTo, [Ping, "Please provide a nick to unignore."]}}};
unignore(_, ReplyTo, Ping, Params, State=#state{ignore=I}) ->
	Nick = string:to_lower(hd(Params)),
	case sets:is_element(Nick, I) of
		false -> {irc, {msg, {ReplyTo, [Ping, "I am not ignoring ", Nick, "."]}}};
		true -> self() ! {state, State#state{ignore=sets:del_element(Nick, I)}},
			{irc, {msg, {ReplyTo, [Ping, "Now listening to ", Nick, "."]}}}
	end.

whoignore(_, ReplyTo, Ping, _, #state{ignore=I}) ->
	case sets:size(I) == 0 of
		true -> {irc, {msg, {ReplyTo, [Ping, "I am not ignoring anyone."]}}};
		false -> {irc, {msg, {ReplyTo, [Ping, "Ignore list: ", string:join(sets:to_list(I), ", ")]}}}
	end.

prefix(_, ReplyTo, Ping, [], _) -> {irc, {msg, {ReplyTo, [Ping, "Please provide a prefix to use for commands."]}}};
prefix(_, ReplyTo, Ping, Params, State=#state{}) ->
	self() ! {state, State#state{prefix=[hd(hd(Params))]}},
	{irc, {msg, {ReplyTo, [Ping, "Prefix set to ", hd(hd(Params))]}}}.

addprefix(_, ReplyTo, Ping, [], _) -> {irc, {msg, {ReplyTo, [Ping, "Please provide an additional prefix to use for commands."]}}};
addprefix(_, ReplyTo, Ping, Params, State=#state{}) ->
	Reply = case State#state.prefix of
		Old when is_number(Old) -> New = [hd(hd(Params)) , Old], ["Prefix set to ",New];
		Old when is_list(Old)   -> New = [hd(hd(Params)) | Old], ["Prefix set to ",New];
		_ -> New = [hd(hd(Params))], io_lib:format("Unknown prefix format ~w", [State#state.prefix])
	end,
	self() ! {state, State#state{prefix=New}},
	{irc, {msg, {ReplyTo, [Ping, Reply]}}}.

remprefix(_, ReplyTo, Ping, [], _) -> {irc, {msg, {ReplyTo, [Ping, "Please provide a prefix to stop using for commands."]}}};
remprefix(_, ReplyTo, Ping, Params, State=#state{}) ->
	ToRemove = hd(hd(Params)),
	Reply = case State#state.prefix of
		Old when is_number(Old) ->
			if
				ToRemove == Old -> New = [];
				true -> New = [Old]
			end,
			["Prefix set to ", New];
		Old when is_list(Old) ->
			New = lists:delete(ToRemove, Old),
			["Prefix set to ", New];
		Old ->
			New = Old, io_lib:format("Unknown prefix format ~w", [Old])
	end,
	self() ! {state, State#state{prefix=New}},
	{irc, {msg, {ReplyTo, [Ping, Reply]}}}.


