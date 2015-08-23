-module(timer).
-compile(export_all).

get_commands() ->
	[
		{"timer", fun timer/5, user}
	].

initialise(T) ->
	case whereis(timer) of
		undefined -> ok;
		Pid ->
			Pid ! quit,
			common:waitfor_gone(timer)
	end,
	spawn(timer, init, []),
	T.

deinitialise(T) ->
	case whereis(timer) of
		undefined -> ok;
		Pid ->
			Pid ! quit,
			common:waitfor_gone(timer)
	end,
	T.


timer(_, ReplyTo, Ping, [    ], _) -> {irc, {msg, {ReplyTo, [Ping, <<"Provide a timer duration in either seconds, minutes:seconds, or hours:minutes:seconds.">>]}}};
timer(O, ReplyTo, Ping, Params, _) ->
	case whereis(timer) of
		undefined -> {irc, {msg, {ReplyTo, [Ping, <<"Timer is currently not running (errored).">>]}}};
		TimerPid ->
			case re:run(hd(Params), <<"^([0-9]+)(?::([0-9]+))?(?::([0-9]+))?$">>, [{capture, all_but_first, binary}]) of
				nomatch -> {irc, {msg, {ReplyTo, [Ping, <<"Provide a correctly-formatted timer duration!">>]}}};
				{match, Times} ->
					Time = lists:foldl(fun(X,S) -> X+60*S end, 0, lists:map(fun binary_to_integer/1, Times)),
					Data = case tl(Params) of
						[] -> {O};
						T -> {O, string:join(T, " ")}
					end,
					TimerPid ! {set, Time, Data},
					{irc, {msg, {ReplyTo, [Ping, <<"Timer set.">>]}}}
			end
	end.


init() ->
	register(timer, self()),
	logging:log(info, "timer", "Loop starting."),
	loop([]),
	logging:log(info, "timer", "Loop ending.").

loop(Timers) ->
	Now = calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(os:timestamp())),
	NewTimers = case Timers of
		[{Time,Datas}|Rest] when Time =< Now -> lists:foreach(fun handle_expiry/1, Datas), Rest;
		_ -> Timers
	end,
	receive
		{set, Length, NewData} ->
			NewDict = case orddict:find(Now + Length, NewTimers) of
				{ok, List} -> orddict:store(Now + Length, [NewData | List], NewTimers);
				error ->      orddict:store(Now + Length, [NewData], NewTimers)
			end,
			loop(NewDict);
		quit -> ok
	after
		1000 -> loop(NewTimers)
	end.

handle_expiry(Data) ->
	case Data of
		{Nick, Message} -> core ! {irc, {msg, {Nick, ["Your timer has expired: ", Message]}}};
		{Nick} -> core ! {irc, {msg, {Nick, "Your timer has expired."}}};
		T -> logging:log(error, "timer", "Unknown timer data format: ~p", [T])
	end.
