-module(timers).
-compile(export_all).

get_commands() ->
	[
		{"timer", fun timer/1, user}
	].

initialise() ->
	case whereis(?MODULE) of
		undefined -> ok;
		Pid ->
			Pid ! quit,
			common:waitfor_gone(?MODULE)
	end,
	spawn(?MODULE, init, []),
	ok.

deinitialise() ->
	case whereis(?MODULE) of
		undefined -> ok;
		Pid ->
			Pid ! quit,
			common:waitfor_gone(?MODULE)
	end.


timer(#{reply:=ReplyTo, ping:=Ping, params:=[    ]}) -> {irc, {msg, {ReplyTo, [Ping, <<"Provide a timer duration in either seconds, minutes:seconds, or hours:minutes:seconds.">>]}}};
timer(#{nick:=O, reply:=ReplyTo, ping:=Ping, params:=Params}) ->
	case whereis(?MODULE) of
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
	register(?MODULE, self()),
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
	{Nick, Msg} = case Data of
		{N, M} -> {N, ["Your timer has expired: ", M]};
		{N} -> {N, "Your timer has expired."}
	end,
	bot ! {request_execute, fun() ->
		core ! {raw, ["WHOIS ", Nick]},
		receive
			{irc, {numeric, {{rpl,whois_user}, Params}}} ->
				core ! {irc, {msg, {Nick, Msg}}};
			{irc, {numeric, {{err,no_such_nick}, _}}} ->
				case lists:member(message, config:get_value(config, [bot, modules])) of
					true -> message:create_message("timer system", Nick, Msg);
					false -> ok
				end
		end
	end}.
