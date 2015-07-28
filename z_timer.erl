-module(z_timer).
-compile(export_all).

get_commands() ->
	[
		{"timer", fun timer/5, dev}
	].

initialize(T) ->
	case whereis(z_timer) of
		undefined -> ok;
		Pid ->
			Pid ! quit,
			common:waitfor_gone(Pid)
	end,
	spawn(z_timer, init, []),
	T.

deinitialize(T) ->
	case whereis(z_timer) of
		undefined -> ok;
		Pid ->
			Pid ! quit,
			common:waitfor_gone(Pid)
	end,
	T.


timer(_, ReplyTo, Ping, [    ], _) -> {irc, {msg, {ReplyTo, [Ping, <<"Provide a timer duration in either seconds, minutes:seconds, or hours:minutes:seconds.">>]}}};
timer(_, ReplyTo, Ping, Params, _) ->
	case re:run(hd(Params), <<"^[0-9]+(:[0-9]+(:[0-9]+)?)?$">>, [{capture, all_but_first, binary}]) of
		nomatch -> {irc, {msg, {ReplyTo, [Ping, <<"Provide a correctly-formatted timer duration!">>]}}};
		{match, Times} ->
			Time = lists:foldr(fun(X,S) -> X+60*S end, 0, Times),
			{irc, {msg, {ReplyTo, [Ping, io_lib:format(<<"~b seconds?">>, [Time])]}}}
	end.


init() ->
	ok
%	,register(self(), z_timer)
	.



















