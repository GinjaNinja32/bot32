-module(command_lock).
-compile(export_all).

-include("definitions.hrl").

get_commands() ->
	[
		{"start", fun start/1, user},
		{"starthere", fun starthere/1, user},
		{"stop", fun stop/1, user}
	].

start(#{reply:=R, ping:=P, params:=[]}) -> {irc, {msg, {R, [P, "Provide a command to start using!"]}}};
start(#{origin:=O, reply:=R, ping:=P, params:=Params}) ->
	config:set_value(temp, [?MODULE, locked, global, O], Params),
	{irc, {msg, {R, [P, "Command '", string:join(Params, " "), "' started."]}}}.
starthere(#{reply:=R, ping:=P, params:=[]}) -> {irc, {msg, {R, [P, "Provide a command to start using here!"]}}};
starthere(#{origin:=O, nick:=N, reply:=R, ping:=P, params:=Params}) ->
	config:set_value(temp, [?MODULE, locked, if R == N -> query; true -> R end, O], Params),
	{irc, {msg, {R, [P, "Command '", string:join(Params, " "), "' started here."]}}}.

handle_event(msg_nocommand, {User, Channel, Tokens}) ->
	case config:get_value(temp, [?MODULE, locked, global, User]) of
		'$none' ->
			UseChan = case config:require_value(config, [bot,nick]) of
				Channel -> query;
				_ -> Channel
			end,
			case config:get_value(temp, [?MODULE, locked, UseChan, User]) of
				'$none' -> ok;
				Locked ->
					case lists:prefix(Locked, Tokens) of
						true -> ok; % the locked command was already prepended!
						false ->
							bot ! {irc, {msg, {User, Channel, Locked ++ Tokens}}}
					end
			end;
		Locked ->
			case lists:prefix(Locked, Tokens) of
				true -> ok; % the locked command was already prepended!
				false ->
					bot ! {irc, {msg, {User, Channel, Locked ++ Tokens}}}
			end
	end;
handle_event(_, _) -> ok.

stop(#{origin:=O, reply:=R, nick:=N, ping:=P}) ->
	case config:get_value(temp, [?MODULE, locked, global, O]) of
		'$none' ->
			UseChan = if R == N -> query; true -> R end,
			case config:get_value(temp, [?MODULE, locked, UseChan, O]) of
				'$none' -> {irc, {msg, {R, [P, "You do not have an active command!"]}}};
				Command ->
					config:del_value(temp, [?MODULE, locked, UseChan, O]),
					{irc, {msg, {R, [P, "Command '", string:join(Command, " "), "' stopped"]}}}
			end;
		Command ->
			config:del_value(temp, [?MODULE, locked, global, O]),
			{irc, {msg, {R, [P, "Command '", string:join(Command, " "), "' stopped"]}}}
	end.
