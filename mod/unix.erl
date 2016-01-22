-module(unix).
-compile(export_all).
-compile({no_auto_import, [now/0]}).

get_commands() ->
	[
		{"ut", fun ut/1, user}
	].

digit(T) -> lists:member(T, "0123456789").

epoch() -> {{1970,1,1}, {0,0,0}}.
now() -> calendar:now_to_universal_time(erlang:now()).

ut(#{reply:=Reply, ping:=Ping, params:=Params}) ->
	{XDT,XUT} = case case Params of
		[] ->
			{num, secsat(now()) - secsat(epoch())};
		[A] ->
			case {digit(hd(A)) orelse hd(A) == $-, lists:all(fun digit/1, tl(A))} of
				{true, true} -> {num, list_to_integer(A)};
				_ -> {str, A}
			end;
		T ->
			{str, string:join(T, " ")}
	end of
		{num, N} ->
			DT = dateat(N + secsat(epoch())),
			{DT, N};
		{str, S} ->
			case lists:map(fun list_to_integer/1, string:tokens(S, "-: ")) of
				[Y,M,D] -> HH = 0, MM = 0, SS = 0;
				[Y,M,D,HH,MM] -> SS = 0;
				[Y,M,D,HH,MM,SS] -> ok
			end,
			DT = {{Y,M,D}, {HH,MM,SS}},
			{DT, secsat(DT) - secsat(epoch())}
	end,
	{irc, {msg, {Reply, [Ping, io_lib:format("UNIX ~b -> ~s", [XUT,dateformat(XDT)])]}}}.

dateformat({{Y,M,D},{HH,MM,SS}}) ->
	io_lib:format("~b-~2..0b-~2..0b ~2..0b:~2..0b:~2..0b UTC", [Y,M,D,HH,MM,SS]).

secsat(T) -> calendar:datetime_to_gregorian_seconds(T).
dateat(T) -> calendar:gregorian_seconds_to_datetime(T).
