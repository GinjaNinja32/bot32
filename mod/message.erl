-module(message).
-compile(export_all).

-include("definitions.hrl").

get_commands() ->
	[
		{"cm", fun check_messages/1, user},
		{"tell", fun new_message/1, user},
		{"showmsg", fun showmsg/1, user},
		{"delmsg", fun delmsg/1, user},
		{"sent", fun sent/1, user}
	].

initialise() ->
	config:offer_value(data, [?MODULE, next_id], 1),
	ok.

handle_event(nick, {_,             N}) -> check_messages_for(N);
handle_event(join, {#user{nick=N}, _}) -> check_messages_for(N);
handle_event(_,_) -> ok.

check_messages(#{nick:=Origin, reply:=ReplyTo, ping:=Ping}) ->
	case check_messages_for(Origin) of
		nomessages -> {irc, {msg, {ReplyTo, [Ping, "You have no new messages."]}}};
		_ -> ok
	end.

new_message(#{reply:=ReplyTo, ping:=Ping, params:=[]}) ->
	{irc, {msg, {ReplyTo, [Ping, "Please provide a recipient and a message."]}}};
new_message(#{reply:=ReplyTo, ping:=Ping, params:=[_]}) ->
	{irc, {msg, {ReplyTo, [Ping, "Please provide a message."]}}};
new_message(#{nick:=Origin, reply:=ReplyTo, ping:=Ping, params:=Params}) ->
	{irc, {msg, {ReplyTo, [Ping, create_message(Origin, hd(Params), string:join(tl(Params), " "))]}}}.

check_messages_for(NickR) ->
	Nick = string:to_lower(NickR),
	lists:foldl(fun
		({ID, {Sender,Recipient,Delivered,Timestamp,Message}}, Acc) when Recipient == Nick andalso not Delivered ->
			{Date,Time} = format_time(Timestamp),
			ThenSecs = calendar:datetime_to_gregorian_seconds(Timestamp),
			NowSecs = calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(os:timestamp())),
			Difference = common:format_time_difference(NowSecs - ThenSecs),
			core ! {irc, {msg, {Nick, [Message, " - ", integer_to_list(ID), " from ", Sender, " at ", Date, $ , Time, " UTC (", Difference, " ago)"]}}},
			config:set_value(data, [?MODULE, messages, ID], {Sender, Recipient, true, Timestamp, Message}),
			if
				Acc == nomessages -> 1;
				true -> Acc + 1
			end;
		(_, Acc) -> Acc
	end, nomessages, config:get_value(data, [?MODULE, messages])).

showmsg(#{origin:=User, nick:=Nick, reply:=Reply, ping:=Ping, params:=[ID]}) ->
	IDN = list_to_integer(ID),
	case config:get_value(data, [?MODULE, messages, IDN]) of
		'$none' -> {irc, {msg, {Reply, [Ping, "That message does not exist."]}}};
		{Sender, Recipient, Delivered, Timestamp, Message} ->
			case permissions:hasperm(User, admin) orelse Nick == Sender orelse Nick == Recipient of
				false -> {irc, {msg, {Reply, [Ping, "You are not authorised to view that message."]}}};
				true ->
					{Date,Time} = format_time(Timestamp),
					{irc, {msg, {Reply, [Ping, io_lib:format("~s - ~b from ~s to ~s at ~s ~s UTC - ~s", [Message, IDN, Sender, Recipient, Date, Time, case Delivered of true -> "Delivered"; false -> "Pending" end])]}}}
			end
	end;
showmsg(#{reply:=Reply, ping:=Ping}) -> {irc, {msg, {Reply, [Ping, "Provide a single numeric message ID to look up."]}}}.

delmsg(#{origin:=User, nick:=Nick, reply:=Reply, ping:=Ping, params:=[ID]}) ->
	IDN = list_to_integer(ID),
	case config:get_value(data, [?MODULE, messages, IDN]) of
		'$none' -> {irc, {msg, {Reply, [Ping, "That message does not exist."]}}};
		{_, _, true, _, _} -> {irc, {msg, {Reply, [Ping, "You cannot delete delivered messages."]}}};
		{Sender, _, _, _, _} ->
			case permissions:hasperm(User, admin) orelse Nick == Sender of
				false -> {irc, {msg, {Reply, [Ping, "You are not authorised to delete that message."]}}};
				true ->
					config:del_value(data, [?MODULE, messages, IDN]),
					{irc, {msg, {Reply, [Ping, io_lib:format("Message ~b deleted.", [IDN])]}}}
			end
	end;
delmsg(#{reply:=Reply, ping:=Ping}) -> {irc, {msg, {Reply, [Ping, "Provide a single numeric message ID to delete."]}}}.

sent(#{nick:=O, reply:=RT, ping:=P}) ->
	OL = string:to_lower(O),
	case catch lists:foldr(fun
		(_, List) when length(List) >= 10 -> throw(List);
		({ID, {Sender, Recipient, false, _, _}}, List) when Sender == OL -> [{ID,Recipient} | List];
		(_, List) -> List
	end, [],  config:get_value(data, [?MODULE, messages])) of
		[] -> {irc, {msg, {RT, [P, "You have not sent any messages that have not been received"]}}};
		List ->
			{irc, {msg, {RT, [P, string:join(lists:map(fun({ID,R}) -> io_lib:format("~b to ~s", [ID, R]) end, List), "; ")]}}}
	end.

format_time({{Y, M, D}, {H, Mi, S}}) ->
	{io_lib:format("~b-~2..0b-~2..0b", [Y, M, D]),
		io_lib:format("~2..0b:~2..0b:~2..0b", [H, Mi, S])}.

create_message(FromR, ToR, Msg) ->
	FromL = string:to_lower(FromR),
	SelfL = string:to_lower(config:require_value(config, [bot, nick])),

	{RealTo, VisTo} = case string:to_lower(ToR) of
		"me" -> {FromL, "you"};
		FromL -> {FromL, "you"};
		To -> {To, ToR}
	end,
	if
		RealTo == SelfL -> "I'm right here, why do you want to send me a message?";
		true ->
			Timestamp = calendar:now_to_universal_time(now()),
			ID = config:mod_get_value(data, [?MODULE, next_id], fun(T) -> T+1 end),
			config:set_value(data, [?MODULE, messages, ID], {FromL, RealTo, false, Timestamp, Msg}),
			[2, integer_to_list(ID), 2, " to ", VisTo, $.]
	end.
