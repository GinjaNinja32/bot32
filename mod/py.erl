-module(py).
-compile(export_all).

-include("definitions.hrl").

get_commands() ->
	[
		{"markov", fun markov/4, user},
		{"pyreload", fun pyreload/4, py},
		{"contexts", fun contexts/4, user}
	].

initialise() ->
	case python:start([{python_path, "./pymod"}, {python, "python3"}]) of
		{ok,Py} ->
			config:set_value(temp, [?MODULE, pypid], Py),
			python:call(Py, python, init, []);
		_ -> ok
	end.

deinitialise() ->
	case config:get_value(temp, [?MODULE, pypid]) of
		'$none' -> ok;
		Py ->
			catch python:call(Py, python, exit, []),
			python:stop(Py)
	end.

pyreload(_, RT, P, _) ->
	case config:get_value(temp, [?MODULE, pypid]) of
		'$none' -> ok;
		Py ->
			catch python:call(Py, python, exit, []),
			python:stop(Py)
	end,
	case python:start([{python_path, "./pymod"}, {python, "python3"}]) of
		{ok, NewPy} ->
			config:set_value(temp, [?MODULE, pypid], NewPy),
			python:call(NewPy, python, init, []),
			{irc, {msg, {RT, [P, "Python reloaded."]}}};
		_ -> core ! {irc, {msg, {RT, [P, "Python failed to start."]}}}
	end.

contexts(_, RT, P, [Word]) ->
	call(contexts, [RT, P, Word], RT);
contexts(_, RT, P, _) -> {irc, {msg, {RT, [P, "Provide a single word!"]}}}.

markov(_, RT, _, Params) ->
	call(markovreply, [RT, string:join(Params, " ")], RT).

call(Func, Args, RT) ->
	case config:get_value(temp, [?MODULE, pypid]) of
		'$none' -> {irc,{msg,{RT,"Could not find python."}}};
		Py -> python:call(Py, python, Func, Args)
	end.

handle_event(msg, {_, Channel, Msg}) ->
	case config:get_value(config, [bot, nick]) of
		Channel -> ok;
		Nick ->
			case re:run(string:join(Msg, " "), [$(, util:regex_escape(Nick) | "|(^|[^a-z0-9])nti([^a-z0-9]|$))"], [caseless, {capture, none}]) of
				match -> Func = markovreply;
				_ -> Func = markov
			end,
			case call(Func, [Channel, string:join(Msg, " ")], Channel) of
				{irc, T} -> core ! {irc, T};
				_ -> ok
			end
	end;
handle_event(_, _) -> ok.




















