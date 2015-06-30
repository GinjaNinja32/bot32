-module(logging).
-export([log/3, log/4, init/0, timestamp/0, loop/1]).

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
			{Name, Log};
		{error, Reason} ->
			io:fwrite("Unable to open log file logs/~s.log: ~s~n", [Name, Reason]), error
	end.

mkentry(Level, What, Message, FormatParams) ->
	io_lib:format("[~s][~s][~s] ~s~n", [timestamp(), Level, What, io_lib:format(Message, FormatParams)]).

init() ->
	case mklog(main) of
		{main, Log} ->
			register(log, self()),
			loop(Log);
		error -> fail
	end.

loop(Log) ->
	receive
		{log, Level, What, Message, FormatParams} ->
			io:fwrite("[~s][~s] ~s~n", [Level, What, io_lib:format(Message, FormatParams)]),
			case file:write(Log, mkentry(Level, What, Message, FormatParams)) of
				ok -> logging:loop(Log);
				{error, Reason} -> io:fwrite("[error][LOGGING] ~s~n", [Reason]), init()
			end;
		stop -> ok
	end.

timestamp() -> format_time(calendar:now_to_local_time(now())).

format_time({{Y, M, D}, {H, Mi, S}}) -> io_lib:format("~b-~2..0b-~2..0b ~2..0b:~2..0b:~2..0b", [Y, M, D, H, Mi, S]).

