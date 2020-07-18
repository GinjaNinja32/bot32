-module(message).
-compile(export_all).

-include("definitions.hrl").

get_commands() ->
	[
		{"cm", fun check_messages/1, user},
		{"tell", fun new_message/1, [{"target",short}, {"message",long}], user},
		{"showmsg", fun showmsg/1, [{"id", integer}], user},
		{"delmsg", fun delmsg/1, [{"id", integer}], user},
		{"sent", fun sent/1, user},
		{"pending", fun pending/1, admin},
		{"pendingfrom", fun pendingfrom/1, admin},
		{"whosent", fun whosent/1, admin},
		{"retargmsg", fun retarget/1, [{"id",integer}, {"target",short}], user},

		{"dset", fun set_discord/1, [{"irc",short},{"discord",long}], admin},
		{"dget", fun get_discord/1, [{"irc",short}], admin},
		{"ddel", fun del_discord/1, [{"irc",short}], admin},
		{"dping", fun ping_discord/1, [], admin}
	].

initialise() ->
	config:offer_value(data, [?MODULE, next_id], 1),
	ok.

now_secs() -> calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(os:timestamp())).

handle_event(nick, {_,             N}) -> check_presence_for(N);
handle_event(join, {#user{nick=N}, _}) -> check_presence_for(N);
handle_event(msg, {#user{nick=N}, _, _}) -> check_presence_for(N);
handle_event(_,_) -> ok.

check_messages(#{nick:=Origin, reply:=ReplyTo, ping:=Ping}) ->
	case check_messages_for(Origin) of
		nomessages -> {irc, {msg, {ReplyTo, [Ping, "You have no new messages."]}}};
		_ -> ok
	end.

new_message(#{nick:=Origin, reply:=ReplyTo, ping:=Ping, params:=[Targets, Message]}) ->
	lists:foldl(fun(Target, Sent) ->
			case lists:member(string:to_lower(Target), Sent) of
				true -> Sent;
				false ->
					core ! {irc, {msg, {ReplyTo, [Ping, create_message(Origin, Target, Message)]}}},
					[string:to_lower(Target) | Sent]
			end
		end, [], string:tokens(Targets, "/,")).

check_presence_for(NickR) ->
	Nick = string:to_lower(NickR),
	Time = now_secs(),
	case config:get_value(temp, [?MODULE, poke, Nick]) of
		T when T /= '$none' andalso T + 3600 > Time -> ok;
		_ ->
			case lists:any(fun({_, {_, Recipient, Del, _, _}}) -> not Del andalso Recipient == Nick end,
					config:get_value(data, [?MODULE, messages])) of
				true ->
					core ! {irc, {msg, {Nick, ["You have pending messages; use '!cm' to check them."]}}},
					config:set_value(temp, [?MODULE, poke, Nick], Time);
				false -> ok
			end
	end.

set_discord(#{reply:=Reply, ping:=Ping, params:=[IRC,Discord]}) ->
	config:set_value(data, [?MODULE, discord, IRC], Discord),
	{irc, {msg, {Reply, [Ping, "added ", IRC, " mapping to discord as ", Discord]}}}.

get_discord(#{reply:=Reply, ping:=Ping, params:=[IRC]}) ->
	case config:get_value(data, [?MODULE, discord, IRC]) of
		'$none' ->
			{irc, {msg, {Reply, [Ping, IRC, " is not mapped to discord"]}}};
		Discord ->
			{irc, {msg, {Reply, [Ping, IRC, " maps to discord as ", Discord]}}}
	end.

del_discord(#{reply:=Reply, ping:=Ping, params:=[IRC]}) ->
	config:del_value(data, [?MODULE, discord, IRC]),
	{irc, {msg, {Reply, [Ping, "removed ", IRC, " mapping to discord"]}}}.

ping_discord(#{reply:=Reply}) ->
	PingMap = lists:foldl(fun({IRC,Discord}, Acc) ->
		case has_messages(IRC) of
			true ->
				case orddict:find(Discord, Acc) of
					{ok, V} -> orddict:store(Discord, lists:umerge([IRC], V), Acc);
					error -> orddict:store(Discord, [IRC], Acc)
				end;
			false -> Acc
		end end, [], config:get_value(data, [?MODULE, discord])),

	Pings = lists:map(fun({Discord, IRCs}) ->
			io_lib:format("~s (~s)", [Discord, string:join(IRCs, ", ")])
		end, PingMap),

	util:groupstrs(
		fun(S) -> core ! {irc, {msg, {Reply, [S, " - You have message(s) on IRC at the nicks in brackets. Use !irc for a webchat link."]}}} end,
		250, Pings, " "),
	ok.

has_messages(NickR) ->
	Nick = string:to_lower(NickR),
	lists:foldl(fun
		({ID, {Sender,Recipient,Delivered,Timestamp,Message}}, Acc) when Recipient == Nick andalso not Delivered ->
			true;
		(_, Acc) -> Acc
	end, false, config:get_value(data, [?MODULE, messages])).

show_messages_for(NickR, Channel) -> check_messages_for(NickR, fun(T) -> core ! {irc,{msg,{Channel,T}}} end).
check_messages_for(NickR) -> check_messages_for(NickR, fun(T) -> core ! {irc,{msg,{NickR,T}}} end).
check_messages_for(NickR, MHandle) ->
	Nick = string:to_lower(NickR),
	lists:foldl(fun
		({ID, {Sender,Recipient,Delivered,Timestamp,Message}}, Acc) when Recipient == Nick andalso not Delivered ->
			{Date,Time} = format_time(Timestamp),
			ThenSecs = calendar:datetime_to_gregorian_seconds(Timestamp),
			NowSecs = calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(os:timestamp())),
			Difference = common:format_time_difference(NowSecs - ThenSecs),
			MHandle([Message, " - ", integer_to_list(ID), " from ", Sender, " at ", Date, $ , Time, " UTC (", Difference, " ago)"]),
			config:set_value(data, [?MODULE, messages, ID], {Sender, Recipient, true, Timestamp, Message}),
			if
				Acc == nomessages -> 1;
				true -> Acc + 1
			end;
		(_, Acc) -> Acc
	end, nomessages, config:get_value(data, [?MODULE, messages])).

showmsg(#{origin:=User, nick:=Nick, reply:=Reply, ping:=Ping, params:=[ID]}) ->
	NickL = string:to_lower(Nick),
	case config:get_value(data, [?MODULE, messages, ID]) of
		'$none' -> {irc, {msg, {Reply, [Ping, "That message does not exist."]}}};
		{Sender, Recipient, Delivered, Timestamp, Message} ->
			case permissions:hasperm(User, admin) orelse NickL == Sender orelse NickL == Recipient of
				false -> {irc, {msg, {Reply, [Ping, "You are not authorised to view that message."]}}};
				true ->
					{Date,Time} = format_time(Timestamp),
					{irc, {msg, {Reply, [Ping, io_lib:format("~s - ~b from ~s to ~s at ~s ~s UTC - ~s", [Message, ID, Sender, Recipient, Date, Time, case Delivered of true -> "Delivered"; false -> "Pending" end])]}}}
			end
	end.

retarget(#{origin:=User, nick:=Nick, reply:=Reply, ping:=Ping, params:=[ID, Target]}) ->
	NickL = string:to_lower(Nick),
	case config:get_value(data, [?MODULE, messages, ID]) of
		'$none' -> {irc, {msg, {Reply, [Ping, "That message does not exist."]}}};
		{_, _, true, _, _} -> {irc, {msg, {Reply, [Ping, "You cannot retarget delivered messages."]}}};
		{Sender, _, _, _, _} ->
			case permissions:hasperm(User, admin) orelse NickL == Sender of
				false -> {irc, {msg, {Reply, [Ping, "You are not authorised to retarget that message."]}}};
				true ->
					config:mod_get_value(data, [?MODULE, messages, ID], fun(T) -> setelement(2, T, string:to_lower(Target)) end),
					{irc, {msg, {Reply, [Ping, "Message retargeted to ", Target, $.]}}}
			end
	end.

delmsg(#{origin:=User, nick:=Nick, reply:=Reply, ping:=Ping, params:=[ID]}) ->
	NickL = string:to_lower(Nick),
	case config:get_value(data, [?MODULE, messages, ID]) of
		'$none' -> {irc, {msg, {Reply, [Ping, "That message does not exist."]}}};
		{_, _, true, _, _} -> {irc, {msg, {Reply, [Ping, "You cannot delete delivered messages."]}}};
		{Sender, _, _, _, _} ->
			case permissions:hasperm(User, admin) orelse NickL == Sender of
				false -> {irc, {msg, {Reply, [Ping, "You are not authorised to delete that message."]}}};
				true ->
					config:del_value(data, [?MODULE, messages, ID]),
					{irc, {msg, {Reply, [Ping, io_lib:format("Message ~b deleted.", [ID])]}}}
			end
	end.

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
	NickNotFound = case lists:member(seen, config:get_value(config, [bot, modules])) of
		true ->
			case config:get_value(data, [seen, RealTo]) of
				{_, _} -> false;
				_ -> true
			end;
		false -> false
	end,
	if
		RealTo == SelfL -> "I'm right here, why do you want to send me a message?";
		NickNotFound -> io_lib:format("I've never seen anyone called ~s.", [ToR]);
		true ->
			Timestamp = calendar:now_to_universal_time(now()),
			ID = config:mod_get_value(data, [?MODULE, next_id], fun(T) -> T+1 end),
			config:set_value(data, [?MODULE, messages, ID], {FromL, RealTo, false, Timestamp, Msg}),
			[2, integer_to_list(ID), 2, " to ", VisTo, $.]
	end.

pending(#{reply:=Reply, ping:=Ping}) ->
	PendingLst = lists:map(fun({X,{_,Trg,_,_,_}}) -> io_lib:format("~b:~s", [X,Trg]) end, lists:filter(fun({_,{_,_,T,_,_}}) -> not T end, config:get_value(data, [?MODULE, messages]))),
	Pending = length(PendingLst),

	Recipients = lists:umerge(lists:map(
			fun({_,{_,T,Del,_,_}}) -> if Del -> []; true -> [T] end end,
		config:get_value(data, [?MODULE, messages]))),

	core ! {irc, {msg, {Reply, [Ping, io_lib:format("There are ~b pending messages to ~b recipients.", [Pending, length(Recipients)])]}}},

	Show = fun(Line) -> core ! {irc, {msg, {Reply, [Ping, string:join(Line, " ")]}}} end,
	pendingf(Show, PendingLst).

pendingfrom(#{reply:=Reply, ping:=Ping}) ->
	PendingLst = lists:map(fun({X,{Send,_,_,_,_}}) -> io_lib:format("~b:~s", [X,Send]) end, lists:filter(fun({_,{_,_,T,_,_}}) -> not T end, config:get_value(data, [?MODULE, messages]))),
	Pending = length(PendingLst),

	Senders = lists:umerge(lists:map(
			fun({_,{T,_,Del,_,_}}) -> if Del -> []; true -> [T] end end,
		config:get_value(data, [?MODULE, messages]))),

	core ! {irc, {msg, {Reply, [Ping, io_lib:format("There are ~b pending messages from ~b senders.", [Pending, length(Senders)])]}}},

	Show = fun(Line) -> core ! {irc, {msg, {Reply, [Ping, string:join(Line, " ")]}}} end,
	pendingf(Show, PendingLst).
	

whosent(#{reply:=Reply, ping:=Ping, params:=Params}) ->
	lists:foldl(fun(T,_) ->
		ID = list_to_integer(T),
		case config:get_value(data, [?MODULE, messages, ID]) of
			'$none' -> core ! {irc, {msg, {Reply, [Ping, io_lib:format("Message ~b does not exist.", [ID])]}}};
			{Src,Dst,_,Timestamp,_} ->
				{Date,Time} = format_time(Timestamp),
				core ! {irc, {msg, {Reply, [Ping, io_lib:format("Message ~b was sent to ~s by ~s at ~s ~s.", [ID, Dst, Src, Date, Time])]}}}
		end end,
		0,
		Params),
	ok.


pendingf(F, Lst) -> pendingf(F, Lst, []).

pendingf(F, [], B) -> F(lists:reverse(B)), ok;
pendingf(F, [A|Rst], B) ->
	if
		length(B) > 19 ->
			F(lists:reverse(B)),
			pendingf(F, Rst, [A]);
		true ->
			pendingf(F, Rst, [A|B])
	end,
	ok.
