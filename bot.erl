-module(bot).
-compile(export_all).
-compile({no_auto_import,[load_module/2]}).

-include("definitions.hrl").

init() ->
	code:add_path("./mod/bin"),
	register(bot, self()),
	{SeedA,SeedB,SeedC}=now(),
	random:seed(SeedA,SeedB,SeedC),

	config:offer_value(config, [permissions], []),
	config:offer_value(config, [bot, nick], "Bot32"),
	config:offer_value(config, [bot, user], "Bot32"),
	config:offer_value(config, [bot, mode], "0"),
	config:offer_value(config, [bot, real], "Bot32"),
	config:offer_value(config, [bot, prefix], "!"),
	config:offer_value(config, [bot, channels], []),
	config:offer_value(config, [bot, modules], []),
	config:offer_value(config, [bot, on_join], []),
	config:offer_value(config, [bot, pass], none),
	config:offer_value(config, [bot, names], []),

	config:set_value(temp, [bot, aliases], []),
	config:set_value(temp, [bot, commands], []),

	util:waitfor(core), % wait for core to startup
	case config:require_value(config, [bot, pass]) of
		none -> ok;
		Pass -> core ! {irc, {pass, Pass}}
	end,
	core ! {irc, {user, {config:require_value(config, [bot, user]), config:require_value(config, [bot, mode]), config:require_value(config, [bot, real])}}},
	core ! {irc, {nick, config:require_value(config, [bot, nick])}},
	receive
		{irc, {numeric, {{rpl, welcome}, _}}} -> ok
	after
		10000 -> throw(connection_failed)
	end,
	timer:sleep(100),
	lists:foreach(fun(T) -> core ! {irc, T} end, config:require_value(config, [bot, on_join])),
	timer:sleep(100), % wait for server auth
	lists:foreach(fun(T) -> core ! {irc, {join, T}} end, config:require_value(config, [bot, channels])),
	logging:log(info, ?MODULE, "starting"),

	config:set_value(temp, [bot, aliases], []),
	config:set_value(temp, [bot, commands], []),

	Modules = config:get_value(config, [bot, modules]),
	config:set_value(config, [bot, modules], []),
	load_modules(Modules),
	loop(),
	logging:log(info, ?MODULE, "stopping").

reinit(_) ->
	register(bot, self()),
	logging:log(info, ?MODULE, "starting"),
	loop(),
	logging:log(info, ?MODULE, "stopping").

notify_error(msg, {#user{nick=N}, Channel, _}) ->
	case config:get_value(config, [bot, nick]) of
		Channel -> {irc, {msg, {N,             "Error!" }}};
		_ ->       {irc, {msg, {Channel, [N, ": Error!"]}}}
	end;
notify_error(X, Y) -> logging:log(error, ?MODULE, "~p : ~p", [X,Y]).

loop() ->
	case receive
		{ircfwd, T} -> {irc, T};
		{irc, {Type, Params}} ->
			logging:log(recv, bot, "{~p, ~p}", [Type, Params]),
			case catch handle_irc(Type, Params) of
				{'EXIT', {Reason, Stack}} -> logging:log(error, ?MODULE, "handle_irc errored ~p (~p), continuing", [Reason, Stack]), notify_error(Type, Params);
				{'EXIT', Term} ->  logging:log(error, ?MODULE, "handle_irc exited ~p, continuing",  [Term]), notify_error(Type, Params);
				T -> T
			end;
		T when is_atom(T) -> T;
		{T, K} when is_atom(T) -> {T, K};
		T -> logging:log(error, ?MODULE, "unknown receive ~p, continuing", [T])
	end of
		{request_execute, Fun} ->
			catch Fun(),
			bot:loop();
		{multi, List} -> lists:foreach(fun(T) -> core ! T end, List), bot:loop();
		{irc, What} -> core ! {irc,What}, bot:loop();
		quit -> ok;
		error -> error;
		ok -> bot:loop();
		update ->
			spawn(common,purge_call,[bot,reinit,[]]),
			ok;
		{update,Chan} ->
			spawn(common,purge_call_report,[bot,reinit,[],Chan]),
			ok;
		S -> logging:log(error, ?MODULE, "unknown code ~p, continuing", [S]), bot:loop()
	end.

parse_command([],_) -> notcommand;
parse_command(Params, IsQuery) ->
	<<FirstChar/utf8, Rest/binary>> = list_to_binary(hd(Params)),
	case lists:member(FirstChar, config:require_value(config, [bot, prefix])) of
		true when Rest /= <<>> -> {binary_to_list(Rest), tl(Params)};
		true -> notcommand;
		false ->
			if
				length(Params) > 1 ->
					BotAliases = [config:require_value(config, [bot, nick]) | config:require_value(config, [bot, names])],
					case lists:any(fun(Alias) -> R=util:regex_escape(Alias), re:run(hd(Params), <<"^", R/binary, "($|[^a-zA-Z0-9])">>, [caseless, {capture, none}]) == match end, BotAliases) of
						true -> case tl(Params) of
								[] -> {[], []};
								_ -> {hd(tl(Params)), tl(tl(Params))}
							end;
						false ->
							if
								IsQuery -> {hd(Params), tl(Params)};
								true -> notcommand
							end
					end;
				IsQuery -> {hd(Params),tl(Params)};
				true -> notcommand
			end
	end.

check_utf8(<<>>) -> true;
check_utf8(<<_/utf8,B/binary>>) -> check_utf8(B);
check_utf8(X) -> io:fwrite("~p~n", [X]), false.

handle_irc(msg, Params={User=#user{nick=Nick}, Channel, Tokens}) ->
	case lists:all(fun(T) -> check_utf8(list_to_binary(T)) end, Tokens) of
		false -> logging:log(utf8, ?MODULE, "Ignoring '~s' due to invalid UTF-8", [string:join(Tokens, " ")]);
		true ->
	case permissions:hasperm(User, ignore) of
		true ->
			logging:log(ignore, ?MODULE, "Ignoring ~s!~s@~s: ~s.", [Nick, User#user.username, User#user.host, string:join(Tokens, " ")]),
			ok;
		false ->
			lists:foreach(fun(Module) ->
					try
						call_or(Module, handle_event, [msg, Params], null)
					catch
						A:B -> logging:log(error, ?MODULE, "Encountered ~p:~p while calling handle_event for ~p!", [A,B,Module])
					end
				end, config:get_value(config, [bot, modules])),
			case config:require_value(config, [bot, nick]) of
				Channel ->
					ReplyChannel = Nick,
					ReplyPing = "",
					case permissions:hasperm(User, admin) of
						true -> ok;
						_ -> permissions:message_all_rank(["Query from ",Nick], string:join(Tokens, " "), pmlog)
					end;
				_ ->
					ReplyChannel = Channel,
					ReplyPing = case config:get_value(data, [call, string:to_lower(Nick)]) of
						'$none' -> Nick ++ ": ";
						T -> T ++ "\x0F: "
					end
			end,
			logging:log(debug2, ?MODULE, "Parsing command: ~p", [Tokens]),
			case parse_command(Tokens, Channel == config:require_value(config, [bot, nick])) of
				{RCommand, RArguments} ->
					logging:log(info, ?MODULE, "Command in ~s from ~s: ~s ~s", [Channel, User#user.nick, RCommand, string:join(RArguments, " ")]),
					{Command, Arguments} = decode_alias(RCommand, RArguments),
					Rank = permissions:rankof(User, ReplyChannel),
					case permissions:hasperm(User, host) of
						true -> handle_host_command(Rank, User, ReplyChannel, ReplyPing, Command, Arguments);
						false ->     handle_command(Rank, User, ReplyChannel, ReplyPing, Command, Arguments)
					end;
				notcommand ->
					lists:foreach(fun(Module) ->
							call_or(Module, do_extras, [Tokens, ReplyChannel, ReplyPing], null),
							call_or(Module, handle_event, [msg_nocommand, Params], null)
						end, config:require_value(config, [bot, modules]))
			end
	end
	end;

handle_irc(nick, {U=#user{nick=OldNick}, NewNick}) ->
	case config:get_value(config, [bot, nick]) of
		OldNick -> config:set_value(config, [bot, nick], NewNick);
		_ ->
			lists:foreach(fun(Module) ->
					call_or(Module, handle_event, [nick, {U, NewNick}], null)
				end, config:require_value(config, [bot, modules]))
	end;

handle_irc(notice, _) -> ok;

handle_irc(numeric, {{rpl,away},_}) -> ok;
handle_irc(numeric, {{A,B},Params}) -> logging:log(info, ?MODULE, "Numeric received: ~p_~p ~s", [A,B,string:join(Params," ")]);

handle_irc(Type, Params) ->
	lists:foreach(fun(Module) ->
				call_or(Module, handle_event, [Type, Params], null)
			end, config:require_value(config, [bot, modules])).

handle_host_command(Rank, User, ReplyTo, Ping, Cmd, Params) ->
	case string:to_lower(Cmd) of
		"update" ->		{update, ReplyTo};
		"help" ->	if Params == [] ->
						core ! {irc, {msg, {User#user.nick, ["builtin host commands: update, reload_all, drop_all, load_mod, drop_mod, reload_mod"]}}};
						true -> ok
					end,
					handle_command(Rank, User, ReplyTo, Ping, Cmd, Params);

		"modules" ->
			{irc, {msg, {ReplyTo, [Ping, string:join(lists:map(fun atom_to_list/1, lists:sort(config:require_value(config, [bot, modules]))), " ")]}}};

		"reload_all" ->
			reload_modules(config:require_value(config, [bot, modules])),
			{irc, {msg, {ReplyTo, [Ping, "Reloaded."]}}};

		"drop_all" ->
			unload_modules(config:require_value(config, [bot, modules])),
			{irc, {msg, {ReplyTo, [Ping, "Unloaded."]}}};

		"load" ->
			case Params of
				[] -> {irc, {msg, {ReplyTo, [Ping, "Provide a module to load."]}}};
				ModuleStrings ->
					load_modules(lists:map(fun erlang:list_to_atom/1, ModuleStrings)),
					{irc, {msg, {ReplyTo, [Ping, "Loaded."]}}}
			end;

		"drop" ->
			case Params of
				[] -> {irc, {msg, {ReplyTo, [Ping, "Provide a module to unload."]}}};
				ModuleStrings ->
					unload_modules(lists:map(fun erlang:list_to_atom/1, ModuleStrings)),
					{irc, {msg, {ReplyTo, [Ping, "Unloaded."]}}}
			end;

		"reload" ->
			case Params of
				[] -> {irc, {msg, {ReplyTo, [Ping, "Provide a module to reload."]}}};
				ModuleStrings ->
					reload_modules(lists:map(fun erlang:list_to_atom/1, ModuleStrings)),
					{irc, {msg, {ReplyTo, [Ping, "Reloaded."]}}}
			end;

		"recompile" ->
			case Params of
				[] -> {irc, {msg, {ReplyTo, [Ping, "Provide a module to recompile."]}}};
				ModuleStrings ->
					recompile_modules(lists:map(fun erlang:list_to_atom/1, ModuleStrings)),
					{irc, {msg, {ReplyTo, [Ping, "Done."]}}}
			end;

		"isalias" ->
			case Params of
				[New] ->
					Msg = case orddict:find(New, config:require_value(temp, [bot, aliases])) of
						{ok, {Real, Spec}} -> io_lib:format("~s is an alias for ~s with argspec ~p.", [New, Real, Spec]);
						{ok, Real} -> io_lib:format("~s is an alias for ~s.", [New, Real]);
						error -> io_lib:format("~s is not an alias.", [New])
					end,
					{irc, {msg, {ReplyTo, [Ping, Msg]}}};
				_ -> {irc, {msg, {ReplyTo, [Ping, "Provide a command to check alias status!"]}}}
			end;

		"unalias" ->
			case Params of
				[New] ->
					Aliases = config:require_value(temp, [bot, aliases]),
					Msg = case orddict:find(New, Aliases) of
						{ok, _} ->
							config:set_value(temp, [bot, aliases], orddict:erase(New, Aliases)),
							"Removed.";
						error -> "Could not find alias."
					end,
					{irc, {msg, {ReplyTo, [Ping, Msg]}}};
				_ -> {irc, {msg, {ReplyTo, [Ping, "Provide an alias to clear!"]}}}
			end;

		"alias" ->
			case Params of
				[New, Real | ArgSpec] ->
					case parse_argspec(ArgSpec) of
						{ok,Spec} ->
							config:set_value(temp, [bot, aliases, New], {Real, Spec}),
							{irc, {msg, {ReplyTo, [Ping, "Done."]}}};
						error -> {irc, {msg, {ReplyTo, [Ping, "Illegal argspec."]}}}
					end;
				_ -> {irc, {msg, {ReplyTo, [Ping, "Provide a command and an alias!"]}}}
			end;

		_ -> handle_command(Rank, User, ReplyTo, Ping, Cmd, Params)
	end.

handle_command(Ranks, User, ReplyTo, Ping, Cmd, Params) ->
	Commands = config:require_value(temp, [bot, commands]),
	Result = case string:to_lower(Cmd) of
		"call" ->
			case Params of
				[] -> {irc, {msg, {ReplyTo, [Ping, "Supply either a nick, or the string 'me', and a name to use!"]}}};
				[_] -> {irc, {msg, {ReplyTo, [Ping, "Supply a name to use!"]}}};
				[RawUsr|Nick] ->
					Usr = case RawUsr of
						"me" -> string:to_lower(User#user.nick);
						_ -> string:to_lower(RawUsr)
					end,
					case case {lists:member(admin, Ranks), string:to_lower(User#user.nick)} of
						{true,_} -> ok;
						{_,Usr} -> ok;
						_ -> false
					end of
						false -> {irc, {msg, {ReplyTo, [Ping, "You are not authorised to do that!"]}}};
						ok ->
							Nickname = string:join(Nick, " "),
							config:set_value(data, [call, Usr], Nickname),
							{irc, {msg, {ReplyTo, [Ping, "Done."]}}}
					end
			end;
		"help" when Params /= [] ->
			case orddict:find(hd(Params), config:require_value(temp, [bot, aliases])) of
				{ok, {HelpCommand,_}} -> ok;
				{ok, HelpCommand} -> ok;
				error -> HelpCommand = hd(Params)
			end,
			HelpTopic = string:join([HelpCommand | tl(Params)], " "),
			case lists:foldl(fun
				(Rank, unhandled) ->
					case case orddict:find(Rank, Commands) of
						{ok, X} -> X;
						error -> orddict:new()
					end of
						[] -> unhandled;
						RankCmds ->
							case orddict:find(HelpCommand, RankCmds) of
								{ok, {Mod,_}} ->
									case call_or(Mod, get_help, [HelpTopic], unhandled) of
										unhandled -> unhandled;
										Strings ->
											core ! {irc, {msg, {User#user.nick, ["Help for '", HelpTopic, "':"]}}},
											lists:foreach(fun(T) -> core ! {irc, {msg, {User#user.nick, T}}} end, Strings), ok
									end;
								error -> unhandled
							end
					end;
				(_, Result) -> Result
			end, unhandled, Ranks) of
				unhandled -> core ! {irc, {msg, {ReplyTo, [Ping, "No help found for '", HelpTopic, "'."]}}};
				_ -> ok
			end,
			ok;
		_ -> lists:foldl(fun
			(Rank, unhandled) ->
				case case orddict:find(Rank, Commands) of
					{ok, X} -> X;
					error -> orddict:new()
				end of
					[] -> unhandled;
					RankCmds ->
						case string:to_lower(Cmd) of
							"help" ->
								do_help_for(User#user.nick, Rank, orddict:fetch_keys(RankCmds)),
								unhandled;
								%core ! {irc, {msg, {Origin, [io_lib:format("~s commands: ",[Rank]), string:join(orddict:fetch_keys(RankCmds), ", "), "."]}}}, unhandled;
							T -> case orddict:find(T, RankCmds) of
								{ok, {_Module,Result}} ->
									ParamMap = #{
											origin => User,
											nick => User#user.nick,
											reply => ReplyTo,
											ping => Ping,
											params => Params
										},
									apply(Result, [ParamMap]);
								error -> unhandled
							end
						end
				end;
			(_, Result) -> Result
		end, unhandled, Ranks)
	end,
	case string:to_lower(Cmd) of
		"help" -> ok;
		_ ->
			case Result of
				unhandled ->
					case alternate_commands([Cmd | Params], config:get_value(config, [bot, modules])) of
						false -> ok; % {irc, {msg, {ReplyTo, [Ping, "Unknown command '",Cmd,"'!"]}}};
						R -> {irc, {msg, {ReplyTo, [Ping, R]}}}
					end;
				_ -> Result
			end
	end.

do_help_for(_, _, []) -> ok;
do_help_for(Origin, Rank, Cmds) when length(Cmds) < 35 -> core ! {irc, {msg, {Origin, [io_lib:format("~s commands: ",[Rank]), string:join(Cmds, ", "), "."]}}};
do_help_for(Origin, Rank, Cmds) ->
	{A,B} = lists:split(35, Cmds),
	core ! {irc, {msg, {Origin, [io_lib:format("~s commands: ",[Rank]), string:join(A, ", "), "."]}}},
	do_help_for(Origin, Rank, B).


alternate_commands(Tokens, Modules) ->
	AltFunctions = lists:foldl(fun(Mod,Alt) ->
			case lists:member({alt_funcs, 0}, Mod:module_info(exports)) of
				true -> Alt ++ Mod:alt_funcs();
				false -> Alt
			end
		end, [], Modules),
	lists:foldl(fun
			(Func, false) -> Func(Tokens);
			(_,Re) -> Re
		end, false, AltFunctions).

load_modules(Modules) -> lists:foreach(fun load_module/1, Modules).
load_module(Module) ->
	Modules = config:require_value(config, [bot, modules]),
	case lists:member(Module, Modules) of
		true -> ok;
		false ->
			logging:log(info, "MODULE", "loading ~p", [Module]),

			% Load commands
			NewCmds = lists:foldl(
				fun
					({Cmd,Fun,Restrict}, Commands) when is_list(Restrict) ->
						lists:foldl(fun(Res, Cmds) ->
							orddict:store(Res, case orddict:find(Res, Cmds) of
									{ok, CmdList} -> orddict:store(Cmd, {Module, Fun}, CmdList);
									error ->         orddict:store(Cmd, {Module, Fun}, orddict:new())
								end, Cmds) end, Commands, Restrict);
					({Cmd,Fun,Restrict}, Commands) ->
						orddict:store(Restrict, case orddict:find(Restrict, Commands) of
								{ok, CmdList} -> orddict:store(Cmd, {Module, Fun}, CmdList);
								error ->         orddict:store(Cmd, {Module, Fun}, orddict:new())
							end, Commands)
			end, config:require_value(temp, [bot, commands]), call_or(Module, get_commands, [], [])),
			config:set_value(temp, [bot, commands], NewCmds),

			% Load aliases
			NewAliases = lists:foldl(
				fun
					({Real, Aliases}, AllAliases) ->
						lists:foldl(fun(A, Al) -> orddict:store(A,Real,Al) end, AllAliases, Aliases)
				end, config:require_value(temp, [bot, aliases]), call_or(Module, get_aliases, [], [])),
			config:set_value(temp, [bot, aliases], NewAliases),

			% Initialise
			call_or(Module, initialise, [], ok),
			config:set_value(config, [bot, modules], [Module | Modules])
	end.

call_or(Mod, Func, Args, Or) ->
	case lists:member({Func,length(Args)}, Mod:module_info(exports)) of
		true -> apply(Mod,Func,Args);
		false -> Or
	end.

unload_modules(Modules) -> lists:foreach(fun unload_module/1, Modules).
unload_module(Module) ->
	Modules = config:require_value(config, [bot, modules]),
	case lists:member(Module, Modules) of
		false -> ok;
		true ->
			logging:log(info, "MODULE", "unloading ~p", [Module]),

			% Remove commands
			Cleaned = orddict:map(fun(_,RankCmds) ->
					orddict:filter(fun(_,{Mod,_}) -> Mod /= Module end, RankCmds)
				end, config:require_value(temp, [bot, commands])),
			Removed = orddict:filter(fun(_,V) -> V /= [] end, Cleaned),
			config:set_value(temp, [bot, commands], Removed),

			% Remove aliases
			NewAliases = lists:foldl(fun({Real,Aliases}, AllAliases) ->
					orddict:filter(fun(K,V) -> not lists:member(K, Aliases) andalso V /= Real end, AllAliases)
				end, config:require_value(temp, [bot, aliases]), call_or(Module, get_aliases, [], [])),
			config:set_value(temp, [bot, aliases], NewAliases),

			% Deinitialise
			call_or(Module, deinitialise, [], ok),
			config:set_value(config, [bot, modules], lists:delete(Module, Modules))
	end.

reload_modules(Modules) -> lists:foreach(fun reload_module/1, Modules).
reload_module(Module) -> unload_module(Module), load_module(Module).

recompile_modules(Modules) -> lists:foreach(fun recompile_module/1, Modules).
recompile_module(Module) ->
	try
		catch unload_module(Module),
		io:fwrite("purge ~p~n", [code:purge(Module)]),
		io:fwrite("compile ~p~n", [compile:file("mod/" ++ atom_to_list(Module), [report_errors, report_warnings, {outdir, "./mod/bin/"}])]),
		io:fwrite("load ~p~n", [code:load_file(Module)]),
		load_module(Module)
	catch
		throw:X -> logging:log(error, "MODULE", "Recompile of ~p threw ~p", [Module, X]);
		error:X -> logging:log(error, "MODULE", "Recompile of ~p errored ~p", [Module, X]);
		exit:X -> logging:log(error, "MODULE", "Recompile of ~p exited ~p", [Module, X])
	end.

decode_alias(Command, Arguments) ->
	Aliases = config:require_value(temp, [bot, aliases]),
	case orddict:find(string:to_lower(Command), Aliases) of
		{ok, {V,Spec}} -> {V, apply_argspec(Spec, Arguments)};
		{ok, V} -> {V, Arguments};
		error -> {Command, Arguments}
	end.

apply_argspec(Spec, Arguments) ->
	Args = lists:flatmap(fun
			({T}) when is_integer(T) -> lists:nthtail(T-1, Arguments);
			(T) when is_integer(T) -> [lists:nth(T, Arguments)];
			(T) -> [T]
		end, Spec),
%	common:debug("debug", "initial ~p, spec ~p, final ~p", [Arguments, Spec, Args]),
	Args.

parse_argspec(Params) ->
	{ok, lists:map(fun(T) ->
			case re:run(T, <<"(\\\\|\\*)([0-9]+)">>, [{capture, all_but_first, binary}]) of
				{match, [<< "*">>, SNum]} -> {binary_to_integer(SNum)};
				{match, [<<"\\">>, SNum]} -> binary_to_integer(SNum);
				nomatch -> T
			end
		end, Params)}.

