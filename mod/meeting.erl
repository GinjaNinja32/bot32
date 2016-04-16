-module(meeting).
-compile(export_all).

-include("definitions.hrl").

get_commands() ->
	[
		{"request", fun request/1, user},
		{"release", fun release/1, user},
		{"transfer", fun transfer/1, user},
		{"frelease", fun frelease/1, meeting}
	].

handle_event(part, {#user{nick=N}, Channel, _}) ->
	case has_control(N, Channel) of
		true -> release_control(Channel, N, "part");
		false -> ok
	end;
handle_event(quit, {#user{nick=N}, _}) ->
	case has_control(N) of
		none -> ok;
		T -> release_control(T, N, "quit")
	end;
handle_event(nick, {#user{nick=N}, New}) ->
	case has_control(N) of
		none -> ok;
		T -> transfer_control(T, N, New, "nick change")
	end;
handle_event(msg_nocommand, {#user{nick=N}, Channel, Tokens}) ->
	case has_control(N) of
		Channel ->
			send2chan(Channel, [hd(N),<<16#feff/utf8>>,tl(N)], Tokens),
			ok;
		_ -> ok
	end;
handle_event(_, _) -> ok.

send2chan(Channel, Nick, Tokens) ->
	send2chan(Channel, Nick, lists:map(fun list_to_binary/1, Tokens), <<>>).

send2chan(Channel, Nick, [], Bin) ->
	core ! {irc, {msg, {Channel, [Nick, $:, $ , Bin]}}};
send2chan(Channel, Nick, [Fst|Rst], Bin) ->
	NewBin = case byte_size(Bin) + byte_size(Fst) of
		T when T > 380 ->
			core ! {irc, {msg, {Channel, [Nick, $:, $ , Bin]}}},
			<<>>;
		_ when Bin == <<>> -> <<>>;
		_ -> <<Bin/binary, 32>>
	end,
	send2chan(Channel, Nick, Rst, <<NewBin/binary, Fst/binary>>).

request(#{nick:=Nick, reply:=Reply, ping:=Ping}) ->
	MeetingChans = config:get_value(config, [?MODULE, channels]),
	case lists:member(Reply, MeetingChans) of
		true ->
			case has_control(Nick) of
				none ->
					case who_controls(Reply) of
						none ->
							gain_control(Reply, Nick);
						_ ->
							config:offer_value(data, [?MODULE, queue, Reply], []),
							config:mod_get_value(data, [?MODULE, queue, Reply], fun(X) -> X ++ [Nick] end),
							{irc, {msg, {Reply, [Nick, " has entered the queue to speak. ", queue_length(Reply)]}}}
					end;
				T ->
					{irc, {msg, {Nick, ["You already have control of ", T, ", release that before requesting another!"]}}}
			end;
		false ->
			{irc, {msg, {Reply, [Ping, "This channel cannot be used with this command."]}}}
	end.

release(#{nick:=Nick, reply:=Reply, ping:=Ping}) ->
	case has_control(Nick) of
		none -> {irc, {msg, {Reply, [Ping, "You do not have control of any channel."]}}};
		T ->
			release_control(T, Nick, "released")
	end.

transfer(#{nick:=Nick, reply:=Reply, ping:=Ping, params:=Params}) ->
	case has_control(Nick) of
		none -> {irc, {msg, {Reply, [Ping, "You do not have control of any channel."]}}};
		T ->
			case Params of
				[New] -> transfer_control(T, Nick, New, "transferred");
				_ -> {irc, {msg, {Nick, ["Usage: transfer [nick]"]}}}
			end
	end.

frelease(#{nick:=Nick, reply:=Chan}) ->
	release_control(Chan, any, ["forced (",Nick,$)]).

queue_length(Channel) ->
	io_lib:format("Queue length: ~b", [length(config:get_value(data, [?MODULE, queue, Channel], []))]).

has_control(Nick, Channel) -> who_controls(Channel) == Nick.
has_control(Nick) ->
	config:get_value(temp, [?MODULE, controlled, Nick], none).
who_controls(Channel) ->
	config:get_value(temp, [?MODULE, controller, Channel], none).

release_control(Channel, any, Reason) ->
	case who_controls(Channel) of
		none -> ok;
		T -> release_control(Channel, T, Reason)
	end;
release_control(Channel, Nick, Reason) ->
	case config:get_value(data, [?MODULE, queue, Channel], []) of
		[] ->
			core ! {irc, {msg, {Channel, [Nick, " releases control: ", Reason]}}},
			config:del_value(temp, [?MODULE, controlled, Nick]),
			config:del_value(temp, [?MODULE, controller, Channel]);
		[New|_] ->
			config:mod_get_value(data, [?MODULE, queue, Channel], fun erlang:tl/1),
			core ! {irc, {msg, {Channel, [Nick, " releases control to ", New, ": ", Reason, "; ", queue_length(Channel)]}}},
			config:del_value(temp, [?MODULE, controlled, Nick]),
			config:set_value(temp, [?MODULE, controlled, New], Channel),
			config:set_value(temp, [?MODULE, controller, Channel], New),
			ok
	end.

transfer_control(Channel, Nick, New, Reason) ->
	core ! {irc, {msg, {Channel, [Nick, " transfers control to ", New, ": ", Reason]}}},
	config:del_value(temp, [?MODULE, controlled, Nick]),
	config:set_value(temp, [?MODULE, controlled, New], Channel),
	config:set_value(temp, [?MODULE, controller, Channel], New).
gain_control(Channel, Nick) ->
	core ! {irc, {msg, {Channel, [Nick, " gains control."]}}},
	config:set_value(temp, [?MODULE, controlled, Nick], Channel),
	config:set_value(temp, [?MODULE, controller, Channel], Nick).
