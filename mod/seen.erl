-module(seen).
-compile(export_all).

-include("definitions.hrl").

get_commands() ->
	[
		{"seen", fun seen/5, user},
		{"debug_seen", fun debug/5, host}
	].

get_data(#state{moduledata=M}) ->
	case orddict:find(seen, M) of
		{ok, Value} -> Value;
		error -> orddict:new()
	end.

set_data(S=#state{moduledata=M}, Data) ->
	S#state{moduledata=orddict:store(seen, Data, M)}.

store_data(Data) ->
	bot ! {setkey, {seen, Data}},
	ok.

store_save_data(Data) ->
	store_data(Data),
	save_seen(Data),
	ok.

initialise(T) ->
	set_data(T, load_seen()).
deinitialise(T) ->
	save_seen(get_data(T)),
	T#state{moduledata=orddict:erase(seen, T#state.moduledata)}.

handle_event(quit, {#user{nick=N}, Reason}, S) -> on_quit(N, Reason, S);
handle_event(part, {#user{nick=N}, Channel, Reason}, S) -> on_part(N, Channel, Reason, S);
handle_event(kick, {#user{nick=N}, WhoKicked, Channel, Reason}, S) -> on_kick(WhoKicked, Channel, Reason, N, S);
handle_event(join, {#user{nick=N}, Channel}, S) -> on_join(N, Channel, S);
handle_event(nick, {#user{nick=Old}, N}, S) -> on_nick(Old, N, S);
handle_event(ctcp, {action, Ch, #user{nick=N}, _}, S) -> on_privmsg(N, Ch, S);
handle_event(_,_,_) -> ok.

%

debug(_, RT, P, _, State) ->
	D=get_data(State),
	logging:log(info, "SEEN", "~p", [D]),
	{irc, {msg, {RT, [P, "Dumped state to console."]}}}.

seen(_, RT, P, [], _) -> {irc, {msg, {RT, [P, "Provide a nick to search for!"]}}};
seen(_, RT, P, Params, State) ->
	LParam = string:to_lower(string:join(Params, " ")),
	D=get_data(State),
	case orddict:filter(fun(N,_) ->
				string:str(string:to_lower(N), LParam) /= 0
			end, D) of
		[] -> {irc, {msg, {RT, [P, "No matching entries found."]}}};
		[{N, {What, When}}] ->
			T = format_tstamp(When),
			{irc, {msg, {RT, [P, N, " was last seen ",What," at ",T]}}};
		T when length(T) =< 10 ->
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
			{irc, {msg, {RT, [P, "Results: ", string:join(Nicks, " "), "; ",N," was last seen ",What," at ",format_tstamp(When)]}}};
		T ->
			Nicks = lists:map(fun({N,_}) -> N end, T),
			{FirstTen, _} = lists:split(10, Nicks),
			{irc, {msg, {RT, [P, "Results: ", string:join(FirstTen, " "), " (", integer_to_list(length(T)-10), " more)"]}}}
	end.

%

on_nick(Old, New, State) ->
	D=get_data(State),
	T = orddict:store(string:to_lower(Old), {["changing nicks to ",New], os:timestamp()}, D),
	store_save_data(orddict:store(string:to_lower(New), {["changing nicks from ",Old], os:timestamp()}, T)).

on_join(User, Channel, State) ->
	D=get_data(State),
	store_save_data(orddict:store(string:to_lower(User), {["joining ", Channel], os:timestamp()}, D)).

on_privmsg(User, Channel, State) ->
	D=get_data(State),
	store_save_data(orddict:store(string:to_lower(User), {["messaging ", Channel], os:timestamp()}, D)).

on_part(User, Channel, Reason, State) ->
	D=get_data(State),
	store_save_data(orddict:store(string:to_lower(User), {["parting ",Channel," stating '",string:join(Reason, " "), $'], os:timestamp()}, D)).

on_quit(User, Reason, State) ->
	D=get_data(State),
	store_save_data(orddict:store(string:to_lower(User), {["quitting IRC stating '",string:join(Reason, " "), $'], os:timestamp()}, D)).

on_kick(User, Channel, Reason, WhoBy, State) ->
	D = get_data(State),
	T = orddict:store(string:to_lower(WhoBy), {["kicking ",User," from ",Channel," stating '",string:join(Reason, " "), $'], os:timestamp()}, D),
	store_save_data(orddict:store(string:to_lower(User), {["being kicked from ",Channel," by ",WhoBy," stating '",string:join(Reason, " "), $'], os:timestamp()}, T)).

%

load_seen() ->
	case file:consult("seen.crl") of
		{ok, [Term]} -> Term;
		{ok, _} -> logging:log(error, "SEEN", "Illegal save format!"), orddict:new();
		{error, E} -> logging:log(error, "SEEN", "Error reading file: ~p!", [E]), orddict:new()
	end.

save_seen(Data) ->
	case file:write_file("seen.crl", io_lib:format("~p.~n", [Data])) of
		ok -> ok;
		T -> logging:log(info, "SEEN", "Save status: ~p", [T])
	end.

format_tstamp(T) ->
	Date={{Y,M,D},{H,Mi,S}} = calendar:now_to_universal_time(T),
	Datetime = io_lib:format("~b-~2..0b-~2..0b ~2..0b:~2..0b:~2..0b UTC", [Y, M, D, H, Mi, S]),
	ThenSecs = calendar:datetime_to_gregorian_seconds(Date),
	Secs = calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(os:timestamp())),
	DiffStr = [common:format_time_difference(Secs - ThenSecs), " ago"],
	[Datetime, " (", DiffStr, ")"].
