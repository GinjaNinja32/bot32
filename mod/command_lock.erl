-module(command_lock).
-compile(export_all).

-include("definitions.hrl").

get_commands() ->
	[
		{"start", fun start/1, user},
		{"stop", fun stop/1, user}
	].

start(#{reply:=R, ping:=P, params:=[]}) -> {irc, {msg, {R, [P, "Provide a command to start using!"]}}};
start(#{origin:=O, reply:=R, ping:=P, params:=Params}) ->
	config:set_value(temp, [?MODULE, locked, O], Params),
	{irc, {msg, {R, [P, "Command '", string:join(Params, " "), "' started."]}}}.

handle_event(msg_nocommand, {User, Channel, Tokens}) ->
	case config:get_value(temp, [?MODULE, locked, User]) of
		'$none' -> ok;
		Locked ->
			case lists:prefix(Locked, Tokens) of
				true -> ok; % the locked command was already prepended!
				false ->
					io:fwrite("~p\n", [Locked ++ Tokens]),
					bot ! {irc, {msg, {User, Channel, Locked ++ Tokens}}}
			end
	end;
handle_event(_, _) -> ok.

stop(#{origin:=O, reply:=R, ping:=P}) ->
	case config:get_value(temp, [?MODULE, locked, O]) of
		'$none' -> {irc, {msg, {R, [P, "You do not have an active command!"]}}};
		Command ->
			config:del_value(temp, [?MODULE, locked, O]),
			{irc, {msg, {R, [P, "Command '", string:join(Command, " "), "' stopped"]}}}
	end.
