-module(logging).
-export([log/3, log/4, init/0, timestamp/0, loop/3]).

log(Level, What, Message) -> log(Level, What, Message, []).
log(Level, What, Message, FormatParams) ->
	case whereis(log) of
		undefined -> Pid = spawn(logging, init, []);
		Pid -> ok
	end,
	Pid ! {log, Level, What, Message, FormatParams},
	ok.

mklog(Name) ->
	file:make_dir("logs"),
	case file:open(["logs/",Name,".log"], [append]) of
		{ok, Log} ->
			file:write(Log, [10, lists:duplicate(25, $#), 10]),
			file:write(Log, io_lib:format("~s Log opened.~n~n", [timestamp()])),
			{ok, Log};
		{error, Reason} ->
			io:fwrite("Unable to open log file logs/~s.log: ~s~n", [Name, Reason]), error
	end.

mkentry(Level, What, Message, FormatParams) ->
	io_lib:format("[~s][~s][~s] ~s~n", [timestamp(), Level, What, io_lib:format(Message, FormatParams)]).

datestr({Date,_}) -> datestr(Date);
datestr({Yr,Mn,Dy}) -> io_lib:format("~b-~b-~b", [Yr,Mn,Dy]).

init() -> init([debug, debug2]).
init(Muted) ->
	LogDate = datestr(calendar:now_to_universal_time(os:timestamp())),
	case mklog(["main-", LogDate]) of
		{ok, Log} ->
			register(log, self()),
			loop(Log, LogDate, Muted);
		error -> fail
	end.

loop(RLog, RLogDate, Muted) ->
	receive
		{all, Level} ->
			NL = lists:delete(Level, Muted),
			io:fwrite("~p~n", [NL]),
			loop(RLog, RLogDate, NL);
		{rll, Level} ->
			case lists:member(Level, Muted) of
				true -> NL = Muted;
				false -> NL = [Level | Muted]
			end,
			io:fwrite("~p~n", [NL]),
			loop(RLog, RLogDate, NL);
		{log, Level, What, Message, FormatParams} ->
			case datestr(calendar:now_to_universal_time(os:timestamp())) of
				RLogDate -> Log = RLog, LogDate = RLogDate;
				DateStr -> 
					case mklog(["main-", DateStr]) of
						{ok, Log} ->
							file:close(RLog),
							LogDate = DateStr;
						error ->
							Log = RLog,
							LogDate = RLogDate
					end
			end,
			case lists:member(Level, Muted) of
				false ->
					io:fwrite("[~s][~s] ~s~n", [Level, What, io_lib:format(Message, FormatParams)])
						;
				true -> ok
			end
			,
			case file:write(Log, mkentry(Level, What, Message, FormatParams)) of
				ok -> logging:loop(Log, LogDate, Muted);
				{error, Reason} -> io:fwrite("[error][LOGGING] ~s~n", [Reason]), init(Muted)
			end;
		stop -> ok
	end.

timestamp() -> format_time(calendar:now_to_local_time(now())).

format_time({{Y, M, D}, {H, Mi, S}}) -> io_lib:format("~b-~2..0b-~2..0b ~2..0b:~2..0b:~2..0b", [Y, M, D, H, Mi, S]).

