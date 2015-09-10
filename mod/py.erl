-module(py).
-compile(export_all).

-include("definitions.hrl").

get_commands() ->
	[
		{"markov", fun markov/5, user},
		{"pyreload", fun pyreload/5, py},
		{"contexts", fun contexts/5, py}
	].

initialise(T) ->
	case python:start([{python_path, "./pymod"}, {python, "python3"}]) of
		{ok,Py} ->
			python:call(Py, python, init, []),
			T#state{moduledata=orddict:store(?MODULE, Py, T#state.moduledata)};
		_ -> T
	end.

deinitialise(T) ->
	case orddict:find(?MODULE, T#state.moduledata) of
		{ok, Py} ->
			catch python:call(Py, python, exit, []),
			python:stop(Py);
		_ -> ok
	end,
	T#state{moduledata=orddict:erase(?MODULE, T#state.moduledata)}.

pyreload(_, RT, P, _, S) ->
	case orddict:find(?MODULE, S#state.moduledata) of
		{ok, Py} ->
			catch python:call(Py, python, exit, []),
			python:stop(Py);
		error -> ok
	end,
	case python:start([{python_path, "./pymod"}, {python, "python3"}]) of
		{ok, NewPy} ->
			python:call(NewPy, python, init, []),
			core ! {irc, {msg, {RT, [P, "Python reloaded."]}}},
			{setkey, {?MODULE, NewPy}};
		_ -> core ! {irc, {msg, {RT, [P, "Python failed to start."]}}}
	end.

contexts(_, RT, P, [Word], S) ->
	call(contexts, [RT, P, Word], RT, S);
contexts(_, RT, P, _, _) -> {irc, {msg, {RT, [P, "Provide a single word!"]}}}.

markov(_, RT, _, Params, S) ->
	call(markovreply, [RT, string:join(Params, " ")], RT, S).

call(Func, Args, RT, S) ->
	case orddict:find(?MODULE, S#state.moduledata) of
		{ok, Py} -> python:call(Py, python, Func, Args);
		error -> {irc,{msg,{RT,"Could not find python."}}}
	end.

handle_event(msg, {_, Channel, Msg}, S) ->
	case S#state.nick of
		Channel -> ok;
		_ ->
			case re:run(string:join(Msg, " "), [$(, util:regex_escape(S#state.nick) | "|(^|[^a-z0-9])nti([^a-z0-9]|$))"], [caseless, {capture, none}]) of
				match -> Func = markovreply;
				_ -> Func = markov
			end,
			case call(Func, [Channel, string:join(Msg, " ")], Channel, S) of
				{irc, T} -> core ! {irc, T};
				_ -> ok
			end
	end;
handle_event(_, _, _) -> ok.




















