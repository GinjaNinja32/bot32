-module(z_admin).
-compile(export_all).

-include("definitions.hrl").

get_commands() ->
	[
		{"admin", fun admin/5, admin},
		{"deadmin", fun deadmin/5, admin},
		{"join", fun join/5, admin},
		{"part", fun part/5, admin},
		{"nick", fun nick/5, admin},
		{"quit", fun quit/5, admin},
		{"speak", fun speak/5, admin},
		{"notice", fun notice/5, admin},
		{"action", fun action/5, admin},
		{"ignore", fun ignore/5, admin},
		{"unignore", fun unignore/5, admin},
		{"whoignore", fun whoignore/5, admin},
		{"prefix", fun prefix/5, admin}
	].

initialise(T) -> T.
deinitialise(T) -> T.

admin(_, ReplyTo, Ping, [], _) ->
	{irc, {msg, {ReplyTo, [Ping, "Please provide an admin to add."]}}};
admin(_, ReplyTo, Ping, Params, State=#state{admins=Admins}) ->
	self() ! {state, State#state{admins=sets:add_element(string:to_lower(hd(Params)), Admins)}},
	{irc, {msg, {ReplyTo, [Ping, "Added ", hd(Params), " to the admins list."]}}}.

deadmin(_, ReplyTo, Ping, [], _) ->
	{irc, {msg, {ReplyTo, [Ping, "Please provide an admin to remove."]}}};
deadmin(_Origin, ReplyTo, Ping, Params, State=#state{admins=Admins}) ->
	self() ! {state, State#state{admins=sets:del_element(string:to_lower(hd(Params)), Admins)}},
	{irc, {msg, {ReplyTo, [Ping, "Removed ", hd(Params), " from the admins list."]}}}.

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
	self() ! {state, State#state{prefix=hd(hd(Params))}},
	{irc, {msg, {ReplyTo, [Ping, "Prefix set to ", hd(hd(Params))]}}}.
