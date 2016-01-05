-module(pymod).
-compile(export_all).

%-include("definitions.hrl").

get_commands() ->
	[
		{"pload", fun pload/1, host},
		{"pdrop", fun pdrop/1, host},
		{"preload", fun preload/1, host}
	].

initialise() _>
	config:set_value(temp, [?MODULE], []).

deinitialise() ->
	io:fwrite("deinit...\n"),
	orddict:map(fun(File, [{pid,Pid}]) ->
		catch python:call(Pid, list_to_atom(File), deinitialise, []),
		python:stop(Pid)
	end, config:get_value(temp, [?MODULE])),
	io:fwrite("done\n").

pload(#{reply:=R, ping:=P, params:=Params}) ->
	io:fwrite("~p\n", [Params]),
	lists:foreach(fun(File) ->
		case file:open(["./pymod/", File, ".py"], [read]) of
			{error, T} -> core ! {irc, {msg, {R, [P, io:fwrite("~s: ~p",[File, T])]}}};
			{ok, IO} ->
				file:close(IO),
				PythonModule = list_to_atom(File),
				case python:start([{python_path, "./pymod"}, {python, "python3"}]) of
					{ok, Py} ->
						config:set_value(temp, [?MODULE, File, pid], Py),
						case python:call(Py, PythonModule, initialise, []) of
							ok -> core ! {irc, {msg, {R, [P, File, " loaded"]}}};
							T -> core ! {irc, {msg, {R, [P, File, io:fwrite(": ~p", [T])]}}}
						end;
					_ -> core ! {irc, {msg, {R, [P, "Failed to start Python!"]}}}
				end
		end
	end, lists:map(fun string:to_lower/1, Params)).

pdrop(#{reply:=R, ping:=P, params:=Params}) ->
	lists:foreach(fun(File) ->
		case config:get_value(temp, [?MODULE, File, pid]) of
			'$none' -> core ! {irc, {msg, {R, [P, File, " is not loaded"]}}};
			Pid ->
				catch python:call(Pid, list_to_atom(File), deinititialise, []),
				python:stop(Pid),
				config:del_value(temp, [?MODULE, File]),
				core ! {irc, {msg, {R, [P, File, " unloaded"]}}}
		end
	end, lists:map(fun string:to_lower/1, Params)).

preload(Map) ->
	pdrop(Map),
	pload(Map).

register_command(Name, Module, Function, Privilege) ->
	config:set_value(temp, [bot, commands, Privilege, Name], {?MODULE, generate_pyfun(Module, Function)}).
unregister_command(Name, Privilege) ->
	config:del_value(temp, [bot, commands, Privilege, Name]).

generate_pyfun(Mod, Func) ->
	fun(#{reply:=R, ping:=P, params:=Params}) ->
		case config:get_value(temp, [?MODULE, atom_to_list(Mod), pid]) of
			'$none' -> {irc, {msg, {R, [P, "Python appears to have disappeared."]}}};
			Py ->
				T = python:call(Py, Mod, Func, [R, P, Params]),
				io:fwrite("python returned ~p\n", [T]),
				T
		end
	end.

handle_event(msg, _) -> ok;
handle_event(Type, Params) ->
	orddict:map(fun(File, [{pid,Pid}]) ->
		case catch python:call(Pid, list_to_atom(File), handle_event, [Type, Params]) of
			ok -> ok;
			T -> io:fwrite("handle_event threw ~p\n", [T])
		end
	end, config:get_value(temp, [?MODULE])).
