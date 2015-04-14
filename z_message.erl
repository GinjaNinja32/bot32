-module(z_message).
-compile(export_all).

-record(state,  {nick, prefix, admins, ignore, dicemode, messages, commands}).

get_commands() ->
	[
		{"cm", fun check_messages/5, user},
		{"tell", fun new_message/5, user}
	].

check_messages(Origin, ReplyTo, Ping, _, State=#state{}) ->
	case check_messages_for(Origin, State) of
		nomessages -> {irc, {msg, {ReplyTo, [Ping, "You have no new messages."]}}};
		_ -> ok
	end.

initialise(T) -> T#state{messages=load_messages()}.
deinitialise(T) -> save_messages(T#state.messages).

new_message(Origin, ReplyTo, Ping, Params, State=#state{}) ->
	{irc, {msg, {ReplyTo, [Ping, new_message(Origin, hd(Params), string:join(tl(Params), " "), State)]}}}.
	
check_messages_for(NickR, State=#state{messages=Messages}) ->
        Nick = string:to_lower(NickR),
        case orddict:find(Nick, Messages) of
                {ok, M} ->
                        lists:foreach(fun({From, Timestamp, Message}) ->
                                        {Date, Time} = format_time(Timestamp),
                                        core ! {irc, {msg, {Nick, [Message, " - From ", From, " at ", Time, " on ", Date]}}}
                                end, M),
                        self() ! {state, State#state{messages=orddict:erase(Nick, Messages)}},
                        ok;
                error -> nomessages
        end.

format_time({{Y, M, D}, {H, Mi, S}}) ->
    {io_lib:format("~b-~2..0b-~2..0b", [Y, M, D]),
        io_lib:format("~2..0b:~2..0b:~2..0b", [H, Mi, S])}.

new_message(FromR, ToR, Msg, State=#state{nick=Self, messages=Messages}) ->
        To = string:to_lower(ToR),
        SelfL = string:to_lower(Self),
        if
                To == SelfL -> "I'm right here, why do you want to send me a message?";
                true ->
                        Timestamp = calendar:now_to_local_time(now()),
                        NewValue = case orddict:find(To, Messages) of
                                {ok, M} ->
                                        Pending = length(M),
                                        if
                                                Pending >= 100 -> toomany;
                                                true -> [{FromR, Timestamp, Msg} | M]
                                        end;
                                error -> [{FromR, Timestamp, Msg}]
                        end,
                        case NewValue of
                                toomany -> [ToR, " has too many pending messages!"];
                                _ ->
                                        self() ! {state, State#state{messages=orddict:store(To, NewValue, Messages)}},
                                        ["Sending a message to ", ToR, " when they arrive."]
                        end
        end.

load_messages() ->
        case file:consult("messages.erl") of
                {ok, Terms} ->
                        common:debug("MSGS", "Loaded."),
                        Terms;
                {error, _} ->
                        common:debug("MSGS", "Creating new."),
                        orddict:new()
        end.

save_messages(Messages) ->
        T = file:write_file("messages.erl", io_lib:format("~p", [Messages])),
        common:debug("MSGS", "~p", [T]).

