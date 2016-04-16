-module(datetime).
-compile(export_all).

get_commands() ->
	[
		{"dt", fun debug/1, [long], user}
	].

debug(#{reply:=Reply, ping:=Ping, params:=[String]}) ->
	PreToks = string:tokens(String, "-: ./"),
	Toks = fix_toks(PreToks),
	case parse_datetime(Toks) of
		error -> {irc, {msg, {Reply, [Ping, "Parse error."]}}};
		{srcdst,_,_} -> {irc, {msg, {Reply, [Ping, "Parse error."]}}};
		{time, H,M,S, Src, Trg} -> {irc, {msg, {Reply, [Ping, format_time({H,M,S}, string:to_upper(Src), string:to_upper(Trg))]}}};
		{date, Y,M,D, Src, Trg} -> {irc, {msg, {Reply, [Ping, format_datetime({{Y,M,D},{0,0,0}}, string:to_upper(Src), string:to_upper(Trg))]}}};
		{datetime, YY,MM,DD,H,M,S, Src, Trg} -> {irc, {msg, {Reply, [Ping, format_datetime({{YY,MM,DD},{H,M,S}}, string:to_upper(Src), string:to_upper(Trg))]}}};
		{utime, UT, Trg} -> {irc, {msg, {Reply, [Ping, format_datetime(UT, "UNIX", Trg)]}}};
		_ -> {irc, {msg, {Reply, [Ping, "Internal error."]}}}
	end.

fix_toks(Tokens) -> lists:reverse(fix_toks2(lists:reverse(Tokens))).

fix_toks2([A=[Y|_],B=[X|_]|Rst]) when ($0 =< Y andalso Y =< $9) andalso not ($0 =< X andalso X =< $9) -> [B++"-"++A|fix_toks2(Rst)];
fix_toks2(Lst) -> Lst.

% OUTPUT

format_time({Hr,Min,Sec}, SrcTZN, "WHEN") ->
	case parse_timezone(string:to_upper(SrcTZN)) of
		false -> "Invalid timezone!";
		SrcTZ ->
			{_,NowTime} = calendar:now_to_universal_time(os:timestamp()),
			Now = datetime2universal({{1,1,1}, NowTime}, {x,0,0,x}),
			Target = datetime2universal({{1,1,1}, {Hr,Min,Sec}}, SrcTZ),

			Diff = Target - Now,

			if
				Diff > 0 -> io_lib:format("It was ~s ~s ago.", [format_for_tz({Hr,Min,Sec}, SrcTZ), common:format_time_difference(-Diff)]);
				true -> io_lib:format("It will be ~s in ~s.", [format_for_tz({Hr,Min,Sec}, SrcTZ), common:format_time_difference(Diff)])
			end
	end;
format_time({Hr,Min,Sec}, SrcTZ, DstTZ) ->
	case parse_timezone(string:to_upper(SrcTZ)) of
		false -> "Invalid source timezone!";
		{_, SrcHr, SrcMin, SrcName} ->
			case parse_timezone(string:to_upper(DstTZ)) of
				false -> "Invalid destination timezone!";
				{_, DstHr, DstMin, DstName} ->
					io_lib:format("~b:~s~s ~s is ~s~s ~s", [Hr, mins(Min), secs(Sec), SrcName, format(Hr-SrcHr+DstHr, Min-SrcMin+DstMin), secs(Sec), DstName])
			end
	end.

format_datetime(Datetime, Src, "WHEN") ->
	case parse_timezone(string:to_upper(Src)) of
		false -> "Invalid source timezone!";
		SrcTZ->
			Universal = datetime2universal(Datetime, SrcTZ),
			NowDT = calendar:now_to_universal_time(os:timestamp()),
			Now = datetime2universal(NowDT, {x,0,0,x}),
			if
				Now - Universal > 0 -> io_lib:format("It was ~s ~s ago.", [format_for_tz(Datetime, SrcTZ), common:format_time_difference(Now - Universal)]);
				true -> io_lib:format("It will be ~s in ~s.", [format_for_tz(Datetime, SrcTZ), common:format_time_difference(Universal - Now)])
			end
	end;
format_datetime(Datetime, Src, Dst) ->
	case parse_timezone(string:to_upper(Src)) of
		false -> "Invalid source timezone!";
		SrcTZ->
			case parse_timezone(string:to_upper(Dst)) of
				false -> "Invalid destination timezone!";
				DstTZ ->
					Universal = datetime2universal(Datetime, SrcTZ),
					DstDatetime = universal2datetime(Universal, DstTZ),
					io_lib:format("~s is ~s.", [format_for_tz(Datetime, SrcTZ), format_for_tz(DstDatetime, DstTZ)])
			end
	end.

format_for_tz(Datetime, unix) -> io_lib:format("UNIX ~b", [Datetime]);
format_for_tz(Datetime, {_,_,_,TZName}) -> io_lib:format("~s ~s", [format_date(Datetime), TZName]).

format_date({{Y, M, D}, {H, Mi, S}}) ->
	io_lib:format("~b-~2..0b-~2..0b ~2..0b:~2..0b~s", [Y, M, D, H, Mi, secs(S)]);
format_date({H, M, S}) ->
	io_lib:format("~2..0b:~2..0b~s", [H, M, secs(S)]).

% PARSING

% Three-element YMD/DMY/MDY
parse_datetime([D1,D2,D3|Rst]) when length(D1)==4 orelse length(D2)==4 orelse length(D3)==4 ->
	case parse_YMD(D1,D2,D3) of
		error -> parse_time([D1,D2,D3|Rst]);
		{Y,M,D} ->
			case parse_time(Rst) of
				{time,HH,MM,SS,Src,Dst} ->
					{datetime, Y,M,D, HH,MM,SS, Src,Dst};
				{srcdst, Src, Dst} ->
					{date, Y,M,D, Src,Dst};
				X ->
					io:fwrite("~p\n", [X]),
					error
			end
	end;
% ISO 8601 YYYYMMDD
parse_datetime([Date|Rst]) when length(Date) == 8 ->
	[Y1,Y2,Y3,Y4,M1,M2,D1,D2] = Date,
	Y = list_to_integer([Y1,Y2,Y3,Y4]),
	M = list_to_integer([M1,M2]),
	D = list_to_integer([D1,D2]),
	case parse_time(Rst) of
		{time,HH,MM,SS,Src,Dst} ->
			{datetime, Y,M,D, HH,MM,SS, Src,Dst};
		{srcdst, Src, Dst} ->
			{date, Y,M,D, Src,Dst};
		X ->
			io:fwrite("~p\n", [X]),
			error
	end;

% Unix time
parse_datetime([UT,Target]) when length(UT) > 6 -> {utime, list_to_integer(UT), Target};
parse_datetime([UT|_]) when length(UT) > 6 -> error;

% No date
parse_datetime(Lst) -> parse_time(Lst).

% HMS
parse_time([H,M,S,Src,Dst]) -> {time, list_to_integer(H), list_to_integer(M), list_to_integer(S), Src,Dst};

parse_time([Time,Src,Dst]) when length(Time) == 4 ->
	[H,H2,M,M2] = Time,
	{time, list_to_integer([H,H2]), list_to_integer([M,M2]), 0, Src, Dst};
% No time
parse_time([Src,Dst]) -> {srcdst, Src, Dst};
parse_time([Trg]) -> {now, Trg};
% Nothing, error.
parse_time(X) ->
			io:fwrite("~p\n", [X]),
			error.

l2i(X) -> list_to_integer(X).

parse_YMD(A,B,C) ->
	case case {length(A), length(B), length(C)} of
		{4,2,2} -> % likely YYYY-MM-DD
			{l2i(A), l2i(B), l2i(C)};
		{X,Y,4} when (X==1 orelse X==2) andalso (Y==1 orelse Y==2) -> % assume DD-MM-YYYY
			{l2i(C), l2i(B), l2i(A)};
		_ ->
			Y = case {is_m(A), is_m(B), is_m(C)} of
				{true, false, false} -> % probably MDY
					{l2i(C), get_m(A), l2i(B)};
				{false, true, false} -> % DMY
					{l2i(C), get_m(B), l2i(A)};
				X ->
					io:fwrite("~p\n", [X]),
					error
			end,
			io:fwrite("~p\n", [Y]),
			Y
	end of
		{__,MM,DD} when MM>12 andalso DD>12 -> error;
		{YY,MM,DD} when MM>12 -> {YY,DD,MM};
		T -> T
	end.

is_m(Str) ->
	UStr = string:to_upper(Str),
	lists:any(fun({_,T}) -> lists:member(UStr, T) end, months()).

get_m(Str) ->
	UStr = string:to_upper(Str),
	case lists:filter(fun({_,T}) -> lists:member(UStr, T) end, months()) of
		[{Num,_}] -> Num;
		X ->
			io:fwrite("~p\n", [X]),
			error
	end.

months() ->
	[
		{1, ["JANUARY","JAN"]},
		{2, ["FEBRUARY","FEB"]},
		{3, ["MARCH","MAR"]},
		{4, ["APRIL","APR"]},
		{5, ["MAY"]},
		{6, ["JUNE","JUN"]},
		{7, ["JULY","JUL"]},
		{8, ["AUGUST","AUG"]},
		{9, ["SEPTEMBER","SEP"]},
		{10, ["OCTOBER","OCT"]},
		{11, ["NOVEMBER","NOV"]},
		{12, ["DECEMBER","DEC"]}
	].

% UTILS/CONVERTERS

get_tz("UNIX") -> unix;
get_tz(Code) ->
	case lists:keyfind(Code, 1, timezone_list:timezones()) of
		{Code,H,M,N} when H < 0 -> {Code,H,-M,N};
		T -> T
	end.

parse_timezone("UNIX") -> unix;
parse_timezone(Code) ->
	case lists:splitwith(fun(T)->T/=$+ andalso T/=$- end, Code) of
		{TZCode,[]} -> get_tz(TZCode);
		{TZCode,Offset} ->
			case parse_offset(tl(Offset)) of
				{Hr,Min} ->
				{H,M} = case hd(Offset) of $+ -> {Hr,Min}; $- -> {-Hr,-Min} end,
					case get_tz(TZCode) of
						{TZCode,TZH,TZM,TZName} -> {x, TZH+H, TZM+M, io_lib:format("~s~s~b:~s", [TZName, [hd(Offset)], abs(H), mins(abs(M))])};
						false -> false
					end;
				false -> false
			end
	end.

parse_offset(TimeStr) ->
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


san(Hr, Min) when  0 > Hr  -> san(Hr+24, Min);
san(Hr, Min) when 24 < Hr  -> san(Hr-24, Min);
san(Hr, Min) when  0 > Min -> san(Hr, Min+60);
san(Hr, Min) when 60 < Min -> san(Hr, Min-60);
san(Hr, Min) -> {Hr, Min}.

format(Hr, Min) when 60 =< Min -> format(Hr+1, Min-60);
format(Hr, Min) when  0  > Min -> format(Hr-1, Min+60);

format(Hr, Min) when Hr < 0 -> [format(Hr+24, Min), " the previous day"];
format(Hr, Min) when Hr > 23 -> [format(Hr-24, Min), " the next day"];
format(Hr, Min) -> io_lib:format("~b:~s", [Hr, mins(Min)]).

mins(N) when N < 10 -> io_lib:format("0~b", [N]);
mins(N) -> io_lib:format("~b", [N]).

secs(0) -> "";
secs(N) when N < 10 -> io_lib:format(":0~b", [N]);
secs(N) -> io_lib:format(":~b", [N]).


datetime2universal(Datetime, unix) -> Datetime + unix_epoch();
datetime2universal(Datetime, {_,HOff,MOff,_}) ->
	Secs = calendar:datetime_to_gregorian_seconds(Datetime),
	Secs - 3600*HOff - 60*MOff.

universal2datetime(Secs, unix) -> Secs - unix_epoch();
universal2datetime(Secs, {_, HOff, MOff, _}) ->
	RSecs = Secs + 3600*HOff + 60*MOff,
	calendar:gregorian_seconds_to_datetime(RSecs).

unix_epoch() ->
	datetime2universal({{1970,1,1},{0,0,0}}, {x,0,0,x}).

