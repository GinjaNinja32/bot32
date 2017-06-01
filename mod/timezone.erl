-module(timezone).
-compile(export_all).

get_commands() ->
	[
		{"time", fun time/1, user}
	].

get_help("time") ->
	[
		"time [time] [src] [dest]: convert [time] from [src] to [dest], e.g. 'time 2:00 EST GMT' to convert 2:00 EST into GMT",
		"time [time] [timezone]: work out how long since/until it was/will be [time] in [timezone], e.g. 'time 20:00 GMT' to find out how long until 20:00 GMT",
		"time [timezone]: find the current time in [timezone]"
	];
get_help(_) -> unhandled.

time(#{reply:=RT, ping:=P, params:=Params}) -> {irc, {msg, {RT, [P, time_convert(Params)]}}}.

time_convert([Dst]) ->
	{_,{Hr,Min,_}} = calendar:now_to_universal_time(os:timestamp()),
	case parse_timezone(string:to_upper(Dst)) of
		false -> "Invalid destination timezone!";
		{_, DstHr, DstMin, DstName} -> io_lib:format("It is currently ~s ~s", [san_format(Hr+DstHr, Min+DstMin), DstName])
	end;

time_convert([]) -> "Provide a time to convert, and a source and destination timezone, a destination timezone, or a time and a timezone.";
time_convert([_,_,_,_|_]) -> "Too many arguments!";
time_convert([Time,TZ]) ->
	case parse_timezone(string:to_upper(TZ)) of
		false -> "Invalid timezone!";
		{_, TZHr, TZMin, TZName} ->
			case parse_time(Time) of
				false -> "Invalid time!";
				{Hr, Min} ->
					{_,{CHr,CMin,_}} = calendar:now_to_universal_time(os:timestamp()),
					{SH, SM} = case {Hr - TZHr - CHr, Min - TZMin - CMin} of
						{A,B} when B < 0 -> {A-1 - (-B div 60), (B rem 60)};
						{A,B} -> {A + (B div 60), (B rem 60)}
					end,
					{H, M} = san(SH, SM),
					if
						H > 12 -> io_lib:format("It was ~b:~s ~s ~s ago.", [Hr, mins(Min), TZName, common:format_time_difference((60-M)*60 + (23-H)*3600)]);
						true -> io_lib:format("It will be ~b:~s ~s in ~s.", [Hr, mins(Min), TZName, common:format_time_difference(M*60 + H*3600)])
					end
			end
	end;
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

get_tz(Code) ->
	case lists:keyfind(Code, 1, timezone_list:timezones()) of
		{Code,H,M,N} when H < 0 -> {Code,H,-M,N};
		T -> T
	end.

parse_timezone(Code) ->
	case lists:splitwith(fun(T)->T/=$+ andalso T/=$- end, Code) of
		{TZCode,[]} -> get_tz(TZCode);
		{TZCode,Offset} ->
				case parse_time(tl(Offset)) of
					{Hr,Min} ->
						{H,M} = case hd(Offset) of $+ -> {Hr,Min}; $- -> {-Hr,-Min} end,
						case get_tz(TZCode) of
							{TZCode,TZH,TZM,TZName} -> {x, TZH+H, TZM+M, io_lib:format("~s~s~b:~s", [TZName, [hd(Offset)], abs(H), mins(abs(M))])};
							false -> false
						end;
					false -> false
				end
	end.

parse_time(TimeStr) ->
	case re:run(TimeStr, "^(2[0-3]|[01][0-9])[0-5][0-9]$", [{capture, none}]) of
		match ->
			{A,B} = lists:split(2, TimeStr),
			{list_to_integer(A), list_to_integer(B)};
		nomatch ->
			case re:run(TimeStr, "^(2[0-3]|[01]?[0-9])(?::([0-5]?[0-9]))?$", [{capture, all_but_first, list}]) of
				{match, [Hr]} -> {list_to_integer(Hr), 0};
				{match, [Hr, Min]} -> {list_to_integer(Hr), list_to_integer(Min)};
				nomatch -> false
			end
	end.

san(Hr, Min) when  0  > Hr  -> san(Hr+24, Min);
san(Hr, Min) when 24 =< Hr  -> san(Hr-24, Min);
san(Hr, Min) when  0  > Min -> san(Hr, Min+60);
san(Hr, Min) when 60 =< Min -> san(Hr, Min-60);
san(Hr, Min) -> {Hr, Min}.

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
