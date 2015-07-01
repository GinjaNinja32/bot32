-module(logging).
-export([log/3, log/4, init/0, timestamp/0, loop/2]).

log(Level, What, Message) -> log(Level, What, Message, []).
log(Level, What, Message, FormatParams) ->
	case whereis(log) of
		undefined -> Pid = spawn(logging, init, []);
		Pid -> ok
	end,
	Pid ! {log, Level, What, Message, FormatParams},
	ok.

mklog(Name) ->
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

init() ->
	LogDate = datestr(calendar:now_to_universal_time(os:timestamp())),
	case mklog(["main-", LogDate]) of
		{ok, Log} ->
			register(log, self()),
			loop(Log, LogDate);
		error -> fail
	end.

loop(RLog, LogDate) ->
	receive
		{log, Level, What, Message, FormatParams} ->
			case datestr(calendar:now_to_universal_time(os:timestamp())) of
				LogDate -> Log = RLog;
				DateStr -> 
					case mklog(["main-", DateStr]) of
						{ok, Log} -> ok;
						error -> Log = RLog
					end
			end,
			io:fwrite("[~s][~s] ~s~n", [Level, What, io_lib:format(Message, FormatParams)]),
			case file:write(Log, mkentry(Level, What, Message, FormatParams)) of
				ok -> logging:loop(Log, LogDate);
				{error, Reason} -> io:fwrite("[error][LOGGING] ~s~n", [Reason]), init()
			end;
		stop -> ok
	end.

timestamp() -> format_time(calendar:now_to_local_time(now())).

format_time({{Y, M, D}, {H, Mi, S}}) -> io_lib:format("~b-~2..0b-~2..0b ~2..0b:~2..0b:~2..0b", [Y, M, D, H, Mi, S]).

