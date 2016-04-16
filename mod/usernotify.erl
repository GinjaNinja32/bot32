-module(usernotify).
-compile(export_all).

-include("definitions.hrl").

get_commands() ->
	[
		{"usernotify", fun usernotify/1, [short], user}
	].

usernotify(#{nick:=SrcNick, reply:=Reply, ping:=Ping, params:=[Nick]}) ->
	LSrcNick = string:to_lower(SrcNick),
	LNick = string:to_lower(Nick),
	case lists:member(LSrcNick, config:get_value(temp, [?MODULE, notify, LNick], [])) of
		true -> {irc, {msg, {Reply, [Ping, "You are already on the list to be notified of ", Nick, $!]}}};
		false ->
			config:mod_get_value(temp, [?MODULE, notify, LNick], fun('$none') -> [LSrcNick]; (Lst) -> [LSrcNick | Lst] end),
			{irc, {msg, {Reply, [Ping, "You will be notified of ", Nick, " when they arrive."]}}}
	end.

handle_event(join, {#user{nick=N}, Chan}) -> see(N, ["joined ", Chan]);
handle_event(msg, {#user{nick=N}, Chan, _}) ->
	case config:get_value(config, [bot, nick]) of
		Chan -> ok;
		_ -> see(N, ["messaged ", Chan])
	end;
handle_event(nick, {#user{nick=Old}, New}) ->
	see(New, ["changed nick from ", Old]),
	see(Old, ["changed nick to ", New]);
handle_event(_, _) -> ok.

see(Nick, What) ->
	lists:foreach(fun(T) ->
			core ! {irc, {msg, {T, ["Notify list: ", Nick, 32, What]}}}
		end, config:get_value(temp, [?MODULE, notify, string:to_lower(Nick)], [])),
	config:set_value(temp, [?MODULE, notify, string:to_lower(Nick)], []).
