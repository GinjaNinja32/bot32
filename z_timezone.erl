-module(z_timezone).
-compile(export_all).

get_commands() ->
	[
		{"time", fun time/5, user}
	].

initialise(T) -> T.
deinitialise(T) -> T.

time(_, RT, P, Params, _) -> {irc, {msg, {RT, [P, time_convert(Params)]}}}.

time_convert(["now"]) -> "Provide a timezone to convert to.";
time_convert(["now", Dst]) ->
	{_,{Hr,Min,_}} = calendar:now_to_universal_time(os:timestamp()),
	case parse_timezone(string:to_upper(Dst)) of
		false -> "Invalid destination timezone!";
		{_, DstHr, DstMin, DstName} -> io_lib:format("It is currently ~s ~s", [san_format(Hr+DstHr, Min+DstMin), DstName])
	end;

time_convert([]) -> "Provide a time to convert, and a source and destination timezone, or the word 'now' and a destination timezone.";
time_convert([_]) -> "Provide a source and destination timezone.";
time_convert([_,_]) -> "Provide a destination timezone.";
time_convert([_,_,_,_|_]) -> "Too many arguments!";
time_convert([Time,Src,Dst]) ->
	case parse_timezone(string:to_upper(Src)) of
		false -> "Invalid source timezone!";
		{_, SrcHr, SrcMin, SrcName} ->
			case parse_timezone(string:to_upper(Dst)) of
				false -> "Invalid destination timezone!";
				{_, DstHr, DstMin, DstName} ->
					case parse_time(Time) of
						false -> "Invalid time!";
						{Hr,Min} ->
							io_lib:format("~b:~s ~s is ~s ~s", [Hr, mins(Min), SrcName, format(Hr-SrcHr+DstHr, Min-SrcMin+DstMin), DstName])
					end
			end
	end.

parse_timezone([$G,$M,$T,$+|N]) -> {H,M} = parse_time(N), {x, H, M, io_lib:format("GMT+~b:~s", [H, mins(M)])};
parse_timezone([$G,$M,$T,$-|N]) -> {H,M} = parse_time(N), {x,-H, M, io_lib:format("GMT-~b:~s", [H, mins(M)])};
parse_timezone(Name) ->
	lists:keyfind(Name, 1, timezone_list:timezones()).

parse_time(TimeStr) ->
	case re:run(TimeStr, "(2[0-3]|[01]?[0-9])(:([0-5][0-9]))?", [{capture, all_but_first, list}]) of
		{match, [Hr]} -> {list_to_integer(Hr), 0};
		{match, [Hr, _, Min]} -> {list_to_integer(Hr), list_to_integer(Min)};
		nomatch -> false
	end.

format(Hr, Min) when 60 =< Min -> format(Hr+1, Min-60);
format(Hr, Min) when  0  > Min -> format(Hr-1, Min+60);

format(Hr, Min) when Hr < 0 -> [format(Hr+24, Min), " the previous day"];
format(Hr, Min) when Hr > 23 -> [format(Hr-24, Min), " the next day"];
format(Hr, Min) -> io_lib:format("~b:~s", [Hr, mins(Min)]).

san_format(Hr, Min) when 60 =< Min -> san_format(Hr+1, Min-60);
san_format(Hr, Min) when  0  > Min -> san_format(Hr-1, Min+60);
san_format(Hr, Min) when 24 =< Hr  -> san_format(Hr-24, Min);
san_format(Hr, Min) when  0  > Hr  -> san_format(Hr+24, Min);
san_format(Hr, Min) -> io_lib:format("~b:~s", [Hr, mins(Min)]).

mins(N) when N < 10 -> io_lib:format("0~b", [N]);
mins(N) -> io_lib:format("~b", [N]).
