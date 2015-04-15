-module(z_message).
-compile(export_all).

-include("definitions.hrl").

get_commands() ->
	[
		{"cm", fun check_messages/5, user},
		{"tell", fun new_message/5, user},
		{"save_msg", fun save_messages/5, admin},
		{"load_msg", fun load_messages/5, admin},
		{"dbg_msg", fun debug_messages/5, admin}
	].

get_data(#state{moduledata=M}) ->
	case orddict:find(z_message, M) of
		{ok, Value} -> Value;
		error -> orddict:new()
	end.

set_data(S=#state{moduledata=M}, Data) ->
	S#state{moduledata=orddict:store(z_message, Data, M)}.

initialise(T) ->
	Messages=load_messages(),
	set_data(T, Messages).

deinitialise(T) -> save_messages(get_data(T)).

check_messages(Origin, ReplyTo, Ping, _, State=#state{}) ->
	case check_messages_for(Origin, State) of
		nomessages -> {irc, {msg, {ReplyTo, [Ping, "You have no new messages."]}}};
		_ -> ok
	end.

new_message(Origin, ReplyTo, Ping, Params, State=#state{}) ->
	{irc, {msg, {ReplyTo, [Ping, new_message(Origin, hd(Params), string:join(tl(Params), " "), State)]}}}.

save_messages(_, ReplyTo, Ping, _, State=#state{}) ->
	deinitialise(State),
	{irc, {msg, {ReplyTo, [Ping, "Saved message data."]}}}.

load_messages(_, ReplyTo, Ping, _, State=#state{}) ->
	self() ! {state, initialise(State)},
	{irc, {msg, {ReplyTo, [Ping, "Loaded message data."]}}}.

debug_messages(_, ReplyTo, Ping, _, State=#state{}) ->
	common:debug("MSGS", "~p", [get_data(State)]),
	{irc, {msg, {ReplyTo, [Ping, "Messages printed to console."]}}}.

check_messages_for(NickR, State=#state{}) ->
	Messages = get_data(State),
        Nick = string:to_lower(NickR),
        case orddict:find(Nick, Messages) of
                {ok, M} ->
                        lists:foreach(fun({From, Timestamp, Message}) ->
                                        {Date, Time} = format_time(Timestamp),
                                        core ! {irc, {msg, {Nick, [Message, " - From ", From, " at ", Time, " on ", Date]}}}
                                end, M),
                        self() ! {state, set_data(State, orddict:erase(Nick, Messages))},
                        ok;
                error -> nomessages
        end.

format_time({{Y, M, D}, {H, Mi, S}}) ->
    {io_lib:format("~b-~2..0b-~2..0b", [Y, M, D]),
        io_lib:format("~2..0b:~2..0b:~2..0b", [H, Mi, S])}.

new_message(FromR, ToR, Msg, State=#state{nick=Self}) ->
	Messages = get_data(State),
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
                                        self() ! {state, set_data(State, orddict:store(To, NewValue, Messages))},
                                        ["Sending a message to ", ToR, " when they arrive."]
                        end
        end.

load_messages() ->
        case file:consult("messages.crl") of
                {ok, [Term]} ->
                        common:debug("MSGS", "Loaded."),
                        Term;
                {error, _} ->
                        common:debug("MSGS", "Creating new."),
                        orddict:new()
        end.

save_messages(Messages) ->
        T = file:write_file("messages.crl", io_lib:format("~p.~n", [Messages])),
        common:debug("MSGS", "~p", [T]).

