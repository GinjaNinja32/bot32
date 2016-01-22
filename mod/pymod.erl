-module(pymod).
-compile(export_all).

%-include("definitions.hrl").

get_commands() ->
	[
		{"pload", fun pload/1, host},
		{"pdrop", fun pdrop/1, host},
		{"preload", fun preload/1, host}
	].

initialise() ->
	config:set_value(temp, [?MODULE], []),
	lists:foreach(fun load/1, config:get_value(config, [?MODULE, modules], [])).

deinitialise() ->
	io:fwrite("deinit...\n"),
	orddict:map(fun(Mod, [{pid,Pid}]) ->
		catch python:call(Pid, Mod, deinitialise, []),
		python:stop(Pid)
	end, config:get_value(temp, [?MODULE])),
	io:fwrite("done\n").

pload(#{reply:=R, ping:=P, params:=Params}) ->
	lists:foreach(fun(File) ->
		case file:open(["./pymod/", File, ".py"], [read]) of
			{error, T} -> core ! {irc, {msg, {R, [P, io:fwrite("~s: ~p",[File, T])]}}};
			{ok, IO} ->
				file:close(IO),

				config:mod_get_value(config, [?MODULE, modules], fun(T) -> lists:umerge([list_to_atom(File)], lists:usort(T)) end),
				X = load(list_to_atom(File)),
				core ! {irc, {msg, {R, [File, io_lib:fwrite(": ~p", [X])]}}}
		end
	end, lists:map(fun string:to_lower/1, Params)).

load(Module) ->
	case config:get_value(temp, [?MODULE, Module, pid]) of
		'$none' ->
			case python:start([{python_path, "./pymod"}, {python, "python3"}]) of
				{ok, Py} ->
					config:set_value(temp, [?MODULE, Module, pid], Py),
					python:call(Py, Module, initialise, []);
				_ -> python_failed
			end;
		_ -> already_loaded
	end.

pdrop(#{reply:=R, ping:=P, params:=Params}) ->
	lists:foreach(fun(File) ->
		Mod = list_to_atom(File),
		core ! {irc, {msg, {R, [P, File, io_lib:format(": ~p", [drop(Mod)])]}}}
	end, lists:map(fun string:to_lower/1, Params)).

drop(Mod) ->
	case config:get_value(temp, [?MODULE, Mod, pid]) of
		'$none' -> not_loaded;
		Pid ->
			catch python:call(Pid, Mod, deinitialise, []),
			python:stop(Pid),
			config:del_value(temp, [?MODULE, Mod]),
			ok
	end.

preload(Map) ->
	pdrop(Map),
	pload(Map).

register_command(Name, Module, Function, Privilege) ->
	config:set_value(temp, [bot, commands, Privilege, Name], {?MODULE, generate_pyfun(Module, Function)}).
unregister_command(Name, Privilege) ->
	config:del_value(temp, [bot, commands, Privilege, Name]).

generate_pyfun(Mod, Func) ->
	fun(#{reply:=R, ping:=P, params:=Params}) ->
		case config:get_value(temp, [?MODULE, Mod, pid]) of
			'$none' -> {irc, {msg, {R, [P, "Python appears to have disappeared."]}}};
			Py -> python:call(Py, Mod, Func, [R, P, Params])
		end
	end.

handle_event(msg, _) -> ok;
handle_event(Type, Params) ->
	orddict:map(fun(File, [{pid,Pid}]) ->
		case catch python:call(Pid, File, handle_event, [Type, Params]) of
			ok -> ok;
			T -> io:fwrite("handle_event threw ~p\n", [T])
		end
	end, config:get_value(temp, [?MODULE])).
