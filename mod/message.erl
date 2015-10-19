-module(message).
-compile(export_all).

-include("definitions.hrl").

get_commands() ->
	[
		{"cm", fun check_messages/4, user},
		{"tell", fun new_message/4, user},
		{"sent", fun sent/4, user}
	].

handle_event(nick, {_,             N}) -> check_messages_for(N);
handle_event(join, {#user{nick=N}, _}) -> check_messages_for(N);
handle_event(_,_) -> ok.

check_messages(Origin, ReplyTo, Ping, _) ->
	case check_messages_for(Origin) of
		nomessages -> {irc, {msg, {ReplyTo, [Ping, "You have no new messages."]}}};
		_ -> ok
	end.

new_message(_, ReplyTo, Ping, []) ->
	{irc, {msg, {ReplyTo, [Ping, "Please provide a recipient and a message."]}}};
new_message(_, ReplyTo, Ping, [_]) ->
	{irc, {msg, {ReplyTo, [Ping, "Please provide a message."]}}};
new_message(Origin, ReplyTo, Ping, Params) ->
	{irc, {msg, {ReplyTo, [Ping, create_message(Origin, hd(Params), string:join(tl(Params), " "))]}}}.

check_messages_for(NickR) ->
        Nick = string:to_lower(NickR),
        case config:get_value(data, [?MODULE, messages, Nick]) of
			'$none' -> nomessages;
			M ->
				logging:log(info, ?MODULE, "Found ~b message(s) for ~s in data", [length(M), Nick]),
				lists:foldr(fun({From, Timestamp, Message},_) ->
					{Date, Time} = format_time(Timestamp),
					ThenSecs = calendar:datetime_to_gregorian_seconds(Timestamp),
					Secs = calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(os:timestamp())),
					DiffStr = common:format_time_difference(Secs - ThenSecs),
                                        core ! {irc, {msg, {Nick, [Message, " - From ", From, " at ", Date, " ", Time, " UTC (",DiffStr," ago)"]}}},
					x
                  end, x, M),
				config:del_value(data, [?MODULE, messages, Nick]),
				ok
        end.

sent(O, RT, P, _) ->
	Data = config:get_value(data, [?MODULE, messages], []),
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

create_message(FromR, ToR, Msg) ->
	FromL = string:to_lower(FromR),
	SelfL = string:to_lower(config:require_value(config, [bot, nick])),

	{RealTo, VisTo, Has, They} = case string:to_lower(ToR) of
		"me" -> {FromL, "you", "have", "you"};
		FromL -> {FromL, "you", "have", "you"};
		To -> {To, ToR, "has", "they"}
	end,
	if
		RealTo == SelfL -> "I'm right here, why do you want to send me a message?";
		true ->
			Timestamp = calendar:now_to_universal_time(now()),
			M = config:get_value(data, [?MODULE, messages, RealTo], []),
			Pending = length(M),
			case if
				Pending >= 100 -> toomany;
				true -> [{FromR, Timestamp, Msg} | M]
			end of
				toomany -> [VisTo, 32, Has, " too many pending messages!"];
				NewValue ->
					config:set_value(data, [?MODULE, messages, RealTo], NewValue),
					["Sending a message to ", VisTo, " when ", They, " arrive."]
			end
	end.
