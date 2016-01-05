-module(modules).
-compile(export_all).

get_commands() ->
	[
		{"modules", fun modules/1, host},
		{"load", fun load/1, host},
		{"drop", fun drop/1, host},
		{"reload", fun reload/1, host},
		{"recompile", fun recompile/1, host}
	].

modules(#{reply:=Reply, ping:=Ping}) ->
	{irc, {msg, {Reply, [Ping,
			string:join(lists:map(fun atom_to_list/1,
					lists:sort(config:require_value(config, [bot, modules]))),
					" ")]}}}.

load(#{reply:=Reply, ping:=Ping, params:=Params}) ->
	case Params of
		[] -> {irc, {msg, {Reply, [Ping, "Provide one or more modules to load."]}}};
		Mods ->
			Status = load_modules(lists:map(fun erlang:list_to_atom/1, Mods)),
			{irc, {msg, {Reply, [Ping, Status]}}}
	end.

drop(#{reply:=Reply, ping:=Ping, params:=Params}) ->
	case Params of
		[] -> {irc, {msg, {Reply, [Ping, "Provide one or more modules to drop, or '*' to drop all."]}}};
		["*"] ->
			Status = drop_modules(config:require_value(config, [bot, modules])),
			{irc, {msg, {Reply, [Ping, Status]}}};
		Mods ->
			Status = drop_modules(lists:map(fun erlang:list_to_atom/1, Mods)),
			{irc, {msg, {Reply, [Ping, Status]}}}
	end.

reload(#{reply:=Reply, ping:=Ping, params:=Params}) ->
	case Params of
		[] -> {irc, {msg, {Reply, [Ping, "Provide one or more modules to reload, or '*' to reload all."]}}};
		["*"] ->
			Status = reload_modules(config:require_value(config, [bot, modules])),
			{irc, {msg, {Reply, [Ping, Status]}}};
		Mods ->
			Status = reload_modules(lists:map(fun erlang:list_to_atom/1, Mods)),
			{irc, {msg, {Reply, [Ping, Status]}}}
	end.

recompile(#{reply:=Reply, ping:=Ping, params:=Params}) ->
	case Params of
		[] -> {irc, {msg, {Reply, [Ping, "Provide one or more modules to recompile, or '*' to reload all."]}}};
		["*"] ->
			Status = recompile_modules(config:require_value(config, [bot, modules])),
			{irc, {msg, {Reply, [Ping, Status]}}};
		Mods ->
			Status = recompile_modules(lists:map(fun erlang:list_to_atom/1, Mods)),
			{irc, {msg, {Reply, [Ping, Status]}}}
	end.

load_modules(List) -> lists:map(fun load_module/1, List).
drop_modules(List) -> lists:map(fun drop_module/1, List).

reload_modules(List) ->
	lists:map(fun(T) ->
			[X,Y,_] = drop_module(T),
			[_,Z,W] = load_module(T),
			[X,Y,$,,$ ,Z,W]
		end, List).

recompile_modules(List) ->
	lists:map(fun(T) ->
			[X,Y,_] = drop_module(T),
			code:purge(T),
			CompileStatus = compile(T),
			code:load_file(T),
			[_,Z,W] = load_module(T),
			[X,Y,$,,$ ,CompileStatus,$,,$ ,Z,W]
		end, List).

compile(Mod) ->
	case compile:file(["mod/",atom_to_list(Mod)], [report,return,{outdir,"./mod/bin"}]) of
		{ok,_} -> "ok";
		{ok,_,[]} -> "ok";
		{ok,_,Warn} -> io_lib:format("~b warning~s", [length(Warn), util:s(length(Warn))]);
		error -> "error";
		{error, Err, []} -> io_lib:format("~b error~s", [length(Err), util:s(length(Err))]);
		{error, Err, Warn} -> io_lib:format("~b error~s, ~b warning~s", [length(Err), util:s(length(Err)), length(Warn), util:s(length(Warn))])
	end.

drop_module(Module) ->
	Stat = case is_loaded(Module) of
		false -> "not loaded";
		true ->
			logging:log(info, ?MODULE, "dropping ~p", [Module]),

			Cleaned = orddict:map(fun(_,RankCmds) ->
					orddict:filter(fun(_,{Mod,_}) -> Mod /= Module end, RankCmds)
				end, config:require_value(temp, [bot, commands])),
			Removed = orddict:filter(fun(_,V) -> V /= [] end, Cleaned),
			config:set_value(temp, [bot, commands], Removed),

			util:call_or(Module, deinitialise, [], ok),
			config:mod_get_value(config, [bot, modules], fun(T) -> lists:delete(Module, T) end),
			"ok"
	end,
	[[atom_to_list(Module),$:,$ ],Stat,[$;,$ ]].

load_module(Module) ->
	Stat = case is_loaded(Module) of
		true -> "already loaded";
		false ->
			logging:log(info, ?MODULE, "loading ~p", [Module]),

			NewCmds = lists:foldl(fun
					({Cmd, Fun, Restrict}, Commands) ->
						orddict:store(Restrict, case orddict:find(Restrict, Commands) of
								{ok, CmdList} -> orddict:store(Cmd, {Module, Fun}, CmdList);
								error ->         orddict:store(Cmd, {Module, Fun}, [     ])
							end, Commands)
				end, config:require_value(temp, [bot, commands]), util:call_or(Module, get_commands, [], [])),
			config:set_value(temp, [bot, commands], NewCmds),

			util:call_or(Module, initialise, [], ok),
			config:mod_get_value(config, [bot, modules], fun(T) -> [Module | T] end),
			"ok"
	end,
	[[atom_to_list(Module),$:,$ ],Stat,[$;,$ ]].

is_loaded(Module) ->
	lists:member(Module, config:require_value(config, [bot, modules])).
