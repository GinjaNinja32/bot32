-module(z_message).
-compile(export_all).

-include("definitions.hrl").

get_commands() ->
	[
		{"cm", fun check_messages/5, user},
		{"tell", fun new_message/5, user},
		{"save_msg", fun save_messages/5, host},
		{"load_msg", fun load_messages/5, host},
		{"dbg_msg", fun debug_messages/5, host}
	].

get_data(#state{moduledata=M}) ->
	case orddict:find(z_message, M) of
		{ok, Value} -> Value;
		error -> orddict:new()
	end.

set_data(S=#state{moduledata=M}, Data) ->
	S#state{moduledata=orddict:store(z_message, Data, M)}.

store_data(Data) ->
	bot ! {setkey, {z_message, Data}},
	ok.

initialise(T) ->
	Messages=load_messages(),
	set_data(T, Messages).

deinitialise(T) ->
	save_messages(get_data(T)),
	T#state{moduledata=orddict:erase(z_message, T#state.moduledata)}.

check_messages(Origin, ReplyTo, Ping, _, State=#state{}) ->
	case check_messages_for(Origin, get_data(State)) of
		nomessages -> {irc, {msg, {ReplyTo, [Ping, "You have no new messages."]}}};
		_ -> ok
	end.

new_message(_, ReplyTo, Ping, [], _) ->
	{irc, {msg, {ReplyTo, [Ping, "Please provide a recipient and a message."]}}};
new_message(_, ReplyTo, Ping, [_], _) ->
	{irc, {msg, {ReplyTo, [Ping, "Please provide a message."]}}};
new_message(Origin, ReplyTo, Ping, Params, State=#state{}) ->
	{irc, {msg, {ReplyTo, [Ping, create_message(Origin, hd(Params), string:join(tl(Params), " "), State#state.nick, get_data(State))]}}}.

save_messages(_, ReplyTo, Ping, _, State=#state{}) ->
	save_messages(get_data(State)),
	{irc, {msg, {ReplyTo, [Ping, "Saved message data."]}}}.

load_messages(_, ReplyTo, Ping, _, _) ->
	store_data(load_messages()),
	{irc, {msg, {ReplyTo, [Ping, "Loaded message data."]}}}.

debug_messages(_, ReplyTo, Ping, _, State=#state{}) ->
	common:debug("MSGS", "~p", [get_data(State)]),
	{irc, {msg, {ReplyTo, [Ping, "Messages printed to console."]}}}.

check_messages_for(NickR, Messages) ->
        Nick = string:to_lower(NickR),
        case orddict:find(Nick, Messages) of
                {ok, M} ->
                        lists:foreach(fun({From, Timestamp, Message}) ->
                                        {Date, Time} = format_time(Timestamp),
					ThenSecs = calendar:datetime_to_gregorian_seconds(Timestamp),
					Secs = calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(os:timestamp())),
					DiffStr = common:format_time_difference(Secs - ThenSecs),
                                        core ! {irc, {msg, {Nick, [Message, " - From ", From, " at ", Date, " ", Time, " UTC (",DiffStr," ago)"]}}}
                                end, M),
			NewData = orddict:erase(Nick, Messages),
			save_messages(NewData),
                        store_data(NewData),
                        ok;
                error -> nomessages
        end.

format_time({{Y, M, D}, {H, Mi, S}}) ->
    {io_lib:format("~b-~2..0b-~2..0b", [Y, M, D]),
        io_lib:format("~2..0b:~2..0b:~2..0b", [H, Mi, S])}.

create_message(FromR, ToR, Msg, Self, Messages) ->
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
					NewData = orddict:store(To, NewValue, Messages),
					save_messages(NewData),
                                        store_data(NewData),
                                        ["Sending a message to ", ToR, " when they arrive."]
                        end
        end.

load_messages() ->
        case file:consult("messages.crl") of
                {ok, [Term]} ->
                        common:debug("MSGS", "Loaded."),
                        Term;
                {error, T} ->
                        common:debug("MSGS", "Creating new (error is ~p)", [T]),
                        orddict:new()
        end.

save_messages(Messages) ->
        T = file:write_file("messages.crl", io_lib:format("~p.~n", [Messages])),
        common:debug("MSGS", "Save status: ~p", [T]).

