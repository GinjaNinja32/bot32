-module(seen).
-compile(export_all).

-include("definitions.hrl").

get_commands() ->
	[
		{"seen", fun seen/1, user}
	].

handle_event(quit, {#user{nick=N}, Reason}) -> see(N, "quitting IRC stating '~s'", [string:join(Reason, " ")]);
handle_event(part, {#user{nick=N}, Channel, Reason}) -> see(N, "parting ~s stating '~s'", [Channel, string:join(Reason, " ")]);
handle_event(kick, {#user{nick=N}, WhoKicked, Channel, Reason}) ->
	see(N, "kicking ~s from ~s stating '~s'", [WhoKicked, Channel, string:join(Reason, " ")]),
	see(WhoKicked, "being kicked from ~s by ~s stating '~s'", [Channel, N, string:join(Reason, " ")]);
handle_event(join, {#user{nick=N}, Channel}) -> see(N, "joining ~s", [Channel]);
handle_event(nick, {#user{nick=Old}, New}) ->
	see(Old, "changing nicks to ~s", [New]),
	see(New, "changing nicks from ~s", [Old]);
handle_event(ctcp, {action, Chan, #user{nick=N}, _}) -> see(N, "messaging ~s", [Chan]);
handle_event(msg, {#user{nick=N}, Chan, _}) ->
	MyNick = config:require_value(config, [bot, nick]),
	if
		Chan /= MyNick -> see(N, "messaging ~s", [Chan]);
		true -> ok
	end;
handle_event(_, _) -> ok.

seen(#{reply:=RT, ping:=P, params:=[]}) -> {irc, {msg, {RT, [P, "Provide a nick to search for!"]}}};
seen(#{reply:=RT, ping:=P, params:=Params}) ->
	LParam = string:to_lower(hd(Params)),
	D = config:get_value(data, [?MODULE]),
	case orddict:filter(fun(N,_) ->
				string:str(string:to_lower(N), LParam) /= 0
			end, D) of
		[] -> {irc, {msg, {RT, [P, hd(Params), " has never been in a channel I can see."]}}};
		[{N, {What, When}}] ->
			T = format_tstamp(When),
			{irc, {msg, {RT, [P, N, " was last seen ",What," at ",T]}}};
		T ->
			{Nicks, {N,{What,When}}} = orddict:fold(
				fun
					(N,{What,When},{Nicks,Recent={_, {_,RecentWhen}}}) ->
						case Recent of
							{none, _} ->
								NewRecent = {N, {What, When}};
							_ ->
								RecentSeconds = calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(RecentWhen)),
								Seconds = calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(When)),
								if
									Seconds > RecentSeconds -> NewRecent = {N, {What, When}};
									true -> NewRecent = Recent
								end
						end,
						{[N|Nicks],NewRecent}
				end, {[], {none, {x,x}}}, T),
			{Extra, FirstTen} = if
				length(Nicks) =< 10 -> {"", Nicks};
				true -> {A,_} = lists:split(10, Nicks), {[" (", integer_to_list(length(Nicks)-10), " more)"], A}
			end,
			{irc, {msg, {RT, [P, "Results: ", string:join(FirstTen, " "), Extra, "; ",N," was last seen ",What," at ",format_tstamp(When)]}}}
	end.

see(User, Message, FormatParams) ->
	D = config:get_value(data, [?MODULE]),
	T = orddict:store(string:to_lower(User), {io_lib:format(Message, FormatParams),os:timestamp()}, D),
	config:set_value(data, [?MODULE], T).

format_tstamp(T) ->
	Date={{Y,M,D},{H,Mi,S}} = calendar:now_to_universal_time(T),
	Datetime = io_lib:format("~b-~2..0b-~2..0b ~2..0b:~2..0b:~2..0b UTC", [Y, M, D, H, Mi, S]),
	ThenSecs = calendar:datetime_to_gregorian_seconds(Date),
	Secs = calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(os:timestamp())),
	DiffStr = [common:format_time_difference(Secs - ThenSecs), " ago"],
	[Datetime, " (", DiffStr, ")"].
