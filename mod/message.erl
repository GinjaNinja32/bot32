-module(message).
-compile(export_all).

-include("definitions.hrl").

get_commands() ->
	[
		{"cm", fun check_messages/5, user},
		{"tell", fun new_message/5, user},
		{"sent", fun sent/5, user},
		{"save_msg", fun save_messages/5, host},
		{"load_msg", fun load_messages/5, host},
		{"dbg_msg", fun debug_messages/5, host}
	].

default_data() -> orddict:new().
data_persistence() -> automatic.
-include("basic_module.hrl").

handle_event_s(nick, {_,             N}, S) -> case check_messages_for(N, get_data(S)) of nomessages -> S; New -> set_data(S, New) end;
handle_event_s(join, {#user{nick=N}, _}, S) -> case check_messages_for(N, get_data(S)) of nomessages -> S; New -> set_data(S, New) end;
handle_event_s(_,_,S) -> S.

check_messages(Origin, ReplyTo, Ping, _, State=#state{}) ->
	case check_messages_for(Origin, get_data(State)) of
		nomessages -> {irc, {msg, {ReplyTo, [Ping, "You have no new messages."]}}};
		NewState -> {setkey, {?MODULE, NewState}}
	end.

new_message(_, ReplyTo, Ping, [], _) ->
	{irc, {msg, {ReplyTo, [Ping, "Please provide a recipient and a message."]}}};
new_message(_, ReplyTo, Ping, [_], _) ->
	{irc, {msg, {ReplyTo, [Ping, "Please provide a message."]}}};
new_message(Origin, ReplyTo, Ping, Params, State=#state{}) ->
	{irc, {msg, {ReplyTo, [Ping, create_message(Origin, hd(Params), string:join(tl(Params), " "), State#state.nick, get_data(State))]}}}.

save_messages(_, ReplyTo, Ping, _, State=#state{}) ->
	save_data(get_data(State)),
	{irc, {msg, {ReplyTo, [Ping, "Saved message data."]}}}.

load_messages(_, ReplyTo, Ping, _, _) ->
	store_data(bot:modload_auto(?MODULE)),
	{irc, {msg, {ReplyTo, [Ping, "Loaded message data."]}}}.

debug_messages(_, ReplyTo, Ping, _, State=#state{}) ->
	logging:log(info, "MSGS", "~p", [get_data(State)]),
	{irc, {msg, {ReplyTo, [Ping, "Messages printed to console."]}}}.

check_messages_for(NickR, Messages) ->
        Nick = string:to_lower(NickR),
        case orddict:find(Nick, Messages) of
           {ok, M} ->
				logging:log(info, ?MODULE, "Found ~b message(s) for ~s in data", [length(M), Nick]),
				lists:foldr(fun({From, Timestamp, Message},_) ->
					{Date, Time} = format_time(Timestamp),
					ThenSecs = calendar:datetime_to_gregorian_seconds(Timestamp),
					Secs = calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(os:timestamp())),
					DiffStr = common:format_time_difference(Secs - ThenSecs),
                                        core ! {irc, {msg, {Nick, [Message, " - From ", From, " at ", Date, " ", Time, " UTC (",DiffStr," ago)"]}}},
					x
                  end, x, M),
			NewData = orddict:erase(Nick, Messages),
			save_data(NewData),
            NewData;
           error -> nomessages
        end.

sent(O, RT, P, _, State) ->
	Data = get_data(State),
	case lists:flatmap(fun({To,List}) ->
				lists:filtermap(fun
						({F,T,S}) when F == O ->
							{Date, Time} = format_time(T),
							ThenSecs = calendar:datetime_to_gregorian_seconds(T),
							Secs = calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(os:timestamp())),
							Diff = common:format_time_difference(Secs - ThenSecs),
							{true, io_lib:format("~s - To ~s at ~s ~s UTC (~s ago)", [S, To, Date, Time, Diff])};
						(_) -> false
					end, List)
			end, Data) of
		[] -> {irc, {msg, {RT, [P, "You have no sent messages."]}}};
		Lst ->
			lists:foldr(fun(T,_) -> core ! {irc, {msg, {O, T}}}, x end, x, Lst),
			ok
	end.

format_time({{Y, M, D}, {H, Mi, S}}) ->
    {io_lib:format("~b-~2..0b-~2..0b", [Y, M, D]),
        io_lib:format("~2..0b:~2..0b:~2..0b", [H, Mi, S])}.

create_message(FromR, ToR, Msg, Self, Messages) ->
	FromL = string:to_lower(FromR),
        SelfL = string:to_lower(Self),

	{RealTo, VisTo, Has, They} = case string:to_lower(ToR) of
		"me" -> {FromL, "you", "have", "you"};
		FromL -> {FromL, "you", "have", "you"};
		To -> {To, ToR, "has", "they"}
	end,
        if
                RealTo == SelfL -> "I'm right here, why do you want to send me a message?";
                true ->
                        Timestamp = calendar:now_to_local_time(now()),
                        NewValue = case orddict:find(RealTo, Messages) of
                                {ok, M} ->
                                        Pending = length(M),
                                        if
                                                Pending >= 100 -> toomany;
                                                true -> [{FromR, Timestamp, Msg} | M]
                                        end;
                                error -> [{FromR, Timestamp, Msg}]
                        end,
                        case NewValue of
                                toomany -> [VisTo, 32, Has, " too many pending messages!"];
                                _ ->
					NewData = orddict:store(RealTo, NewValue, Messages),
					save_data(NewData),
                                        store_data(NewData),
                                        ["Sending a message to ", VisTo, " when ", They, " arrive."]
                        end
        end.

save_data(Messages) ->
        T = file:write_file("modules/message.crl", io_lib:format("~p.~n", [Messages])),
        logging:log(info, "MSGS", "Save status: ~p", [T]).
