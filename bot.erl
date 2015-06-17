-module(bot).
-compile(export_all).
-compile({no_auto_import,[load_module/2]}).

-include("definitions.hrl").

-record(config, {nick, prefix, permissions, ignore, user, mode, real, channels, on_join, modules}).

waitfor(Ident) ->
	case whereis(Ident) of
		undefined ->
			timer:sleep(100),
			waitfor(Ident);
		_ -> ok
	end.

init() ->
	register(bot, self()),
	{SeedA,SeedB,SeedC}=now(),
	random:seed(SeedA,SeedB,SeedC),
	BasePerms = case file:consult("permissions.crl") of
		{ok, [Perms]} -> Perms;
		_ -> orddict:new()
	end,
	BaseConfig = #config{nick="Bot32", prefix=$!, permissions=BasePerms, user="Bot32", mode="0", real="Bot32", channels=sets:new(), ignore=sets:new(), on_join=[], modules=[z_basic]},
	case file:consult("bot_config.crl") of
		{error, Reason} ->
			common:debug("BOT", "Failed to load config file: ~p.", [Reason]),
			UseConfig = BaseConfig;
		{ok, Terms} ->
			UseConfig = lists:foldl(fun(Option, Config=#config{permissions=Perms, on_join=OJ, channels=C, ignore=I, modules=M}) ->
					case Option of
						{nick, Nick}		when is_list(Nick) orelse is_binary(Nick)	-> Config#config{nick=Nick};
						{user, User}		when is_list(User) orelse is_binary(User)	-> Config#config{user=User};
						{mode, Mode}		when is_list(Mode) orelse is_binary(Mode)	-> Config#config{mode=Mode};
						{real, Real}		when is_list(Real) orelse is_binary(Real)	-> Config#config{real=Real};

						{prefix, Prefix}	when is_integer(Prefix)				-> Config#config{prefix=Prefix};

						{permission, {N,U,H}, P}	when is_list(N) andalso is_list(U) andalso is_list(H) andalso is_atom(P) ->
														NUH = {string:to_lower(N),U,H},
														NewPerms = case orddict:find(NUH, Perms) of
															{ok, V} ->
																case lists:member(P, V) of
																	true -> V;
																	false -> [P | V]
																end;
															error -> [user, P]
														end,
														Config#config{permissions=orddict:store(NUH, NewPerms, Perms)};

						{channel, Channel}	when is_list(Channel) orelse is_binary(Channel)	-> Config#config{channels=sets:add_element(Channel, C)};
						{channels, Channels}	when is_list(Channels)				-> Config#config{channels=lists:foldl(fun sets:add_element/2, C, Channels)};

						{ignore, Ignore}	when is_list(Ignore) orelse is_binary(Ignore)	-> Config#config{ignore=sets:add_element(string:to_lower(Ignore), I)};
						{ignores, Ignores}	when is_list(Ignores)				-> Config#config{ignore=lists:foldl(fun sets:add_element/2, I, lists:map(fun string:to_lower/1, Ignores))};

						{module, Mod}		when is_atom(Mod)				-> Config#config{modules = [Mod | M]};
						{modules, Mods}		when is_list(Mods)				-> Config#config{modules=lists:foldl(
																fun	(Mod, MX) when is_atom(Mod) -> [Mod|MX];
																	(Mod, _) -> common:debug("BOT", "Non-atomic module ~p specified!", [Mod])
																 end, M, Mods)};

						{on_join, Cmd}		when is_tuple(Cmd)				-> Config#config{on_join = [Cmd | OJ]};

						T -> common:debug("BOT", "Failed to parse config line ~p!", [T]), Config
					end
				end, BaseConfig, Terms)
	end,
	waitfor(core), % wait for core to startup
	core ! {irc, {user, {UseConfig#config.user, UseConfig#config.mode, UseConfig#config.real}}},
	core ! {irc, {nick, UseConfig#config.nick}},
	timer:sleep(250),
	lists:foreach(fun(T) ->
			core ! {irc, T}
		end, UseConfig#config.on_join),
	timer:sleep(50), % wait for server auth
	lists:foreach(fun(T) -> core ! {irc, {join, T}} end, sets:to_list(UseConfig#config.channels)),
	common:debug("BOT", "starting"),
	State = load_modules(UseConfig#config.modules,
			    #state{
				nick     = UseConfig#config.nick,
				prefix   = UseConfig#config.prefix,
				permissions = UseConfig#config.permissions,
				ignore   = UseConfig#config.ignore,
				commands = orddict:new(),
				moduledata  = orddict:new(),
				modules = sets:new()
			    }),
	case loop(State) of
		FinalState=#state{} ->
			X = file:write_file("permissions.crl", io_lib:format("~p.~n", [FinalState#state.permissions])),
			common:debug("BOT", "permissions save: ~p", [X]),
			lists:foreach(fun(Module) ->
					apply(Module, deinitialise, [FinalState])
				end, sets:to_list(FinalState#state.modules)),
			common:debug("BOT", "quitting");
		T -> common:debug("BOT", "quitting under condition ~p", [T])
	end.

reinit(State) ->
	register(bot, self()),
	common:debug("BOT", "starting"),
	case loop(State) of
		FinalState=#state{} ->
			lists:foreach(fun(Module) ->
					apply(Module, deinitialise, [FinalState])
				end, sets:to_list(FinalState#state.modules)),
			common:debug("BOT", "quitting");
		T -> common:debug("BOT", "quitting under condition ~p", [T])
	end.

notify_error(msg, {#user{nick=N}, MyNick, _}, #state{nick=MyNick}) -> {irc, {msg, {N, "Error!"}}};
notify_error(msg, {#user{nick=N}, Channel, _}, _) -> {irc, {msg, {Channel, [N, ": Error!"]}}};
notify_error(X, Y, _) -> common:debug("BOT", "~p : ~p", [X,Y]).

loop(State = #state{}) ->
	case receive
		{irc, {Type, Params}} ->
			try
				handle_irc(Type, Params, State)
			catch
				throw:T -> common:debug("BOT", "handle_irc threw ~p, continuing",   [T]), notify_error(Type, Params, State);
				error:T -> common:debug("BOT", "handle_irc errored ~p, continuing", [T]), notify_error(Type, Params, State);
				exit:T ->  common:debug("BOT", "handle_irc exited ~p, continuing",  [T]), notify_error(Type, Params, State)
			end;
		T when is_atom(T) -> T;
		{T, K} when is_atom(T) -> {T, K};
		T -> common:debug("BOT", "unknown receive ~p, continuing", [T])
	end of
		{multi, List} -> lists:foreach(fun(T) -> core ! T end, List), loop(State);
		{irc, What} -> core ! {irc,What}, loop(State);
		quit -> State;
		error -> error;
		ok -> loop(State);
		{state, S = #state{}} -> loop(S);
		{setkey, {Key, Val}} -> loop(State#state{moduledata = orddict:store(Key, Val, State#state.moduledata)});
		update -> spawn(common,purge_call,[bot,reinit, State]), ok;
		S -> common:debug("BOT", "unknown code ~p, continuing", [S]), loop(State)
	end.

message_admins(Category, Msg, Admins) ->
	common:debug("ADMIN", [Category, ": ", Msg]),
	lists:foreach(fun(T) ->
			core ! {irc, {msg, {T, [Category, ": ", Msg]}}}
		end, sets:to_list(Admins)),
	ok.

message_all_rank(Category, Msg, Rank, Permissions) ->
	common:debug(Rank, "~s: ~s", [Category, Msg]),
	lists:foreach(fun({N,_U,_H}) ->
			core ! {irc, {msg, {N, [Category, ": ", Msg]}}}
		end, orddict:fetch_keys(orddict:filter(fun(_,V) -> V == Rank end, Permissions))).

is_ignored(#user{nick=N}, Ignored) ->
	sets:is_element(string:to_lower(N), Ignored).

rankof(#user{nick=N,username=U,host=H}, Permissions) ->
	case orddict:find({string:to_lower(N),U,H}, Permissions) of
		{ok, T} -> T;
		error -> [user]
	end.

hasperm(_, user, _) -> true;
hasperm(#user{nick=N,username=U,host=H}, Perm, Permissions) ->
	case orddict:find({string:to_lower(N),U,H}, Permissions) of
		{ok, T} -> lists:member(Perm, T);
		error -> false
	end.

handle_irc(msg, {User=#user{nick=Nick}, Channel, Tokens}, State=#state{nick=MyNick, prefix=Prefix, permissions=Permissions, ignore=Ignored, modules=M}) ->
	case sets:is_element(z_seen, M) of
		true -> if
				Channel /= MyNick -> z_seen:on_privmsg(Nick, Channel, State);
				true -> ok
			end;
		_ -> ok
	end,
	case is_ignored(User, Ignored) of
		true -> ok;
		false ->
			case Channel of
				MyNick ->
					ReplyChannel = Nick,
					ReplyPing = "",
					case hasperm(User, admin, Permissions) of
						true -> ok;
						_ -> message_all_rank(["Query from ",Nick], string:join(Tokens, " "), admin, Permissions)
					end;
				_ ->
					ReplyChannel = Channel,
					ReplyPing = Nick ++ ": "
			end,
			case hd(Tokens) of
				[Prefix | Command] ->
					Rank = rankof(User, Permissions),
					case hasperm(User, host, Permissions) of
						true -> handle_host_command(Rank, Nick, ReplyChannel, ReplyPing, Command, tl(Tokens), State);
						false ->      handle_command(Rank, Nick, ReplyChannel, ReplyPing, Command, tl(Tokens), State)
					end;
				X ->
					case lists:prefix(string:to_lower(MyNick), string:to_lower(X)) of
						true ->
							Rank = rankof(User, Permissions),
							case hasperm(User, host, Permissions) of
								true -> handle_host_command(Rank, Nick, ReplyChannel, ReplyPing, hd(tl(Tokens)), tl(tl(Tokens)), State);
								false ->      handle_command(Rank, Nick, ReplyChannel, ReplyPing, hd(tl(Tokens)), tl(tl(Tokens)), State)
							end;
						false -> ok
					end
			end
	end;

handle_irc(ctcp, {Type, #user{nick=Nick}, _Message}, _State) ->
	case Type of
		version -> {irc, {ctcp_re, {version, Nick, ?VERSION}}};
		action -> ok;
		_ -> common:debug("BOT", "Unknown CTCP message ~p, continuing", [Type])
	end;

handle_irc(nick, {#user{nick=MyNick}, NewNick}, State=#state{nick=MyNick}) -> {state, State#state{nick=NewNick}};
handle_irc(nick, {#user{nick=Old}, N}, S=#state{modules=M}) ->
	case sets:is_element(z_message, M) of
		true -> z_message:check_messages_for(N, z_message:get_data(S));
		_ -> ok
	end,
	case sets:is_element(z_seen, M) of
		true -> z_seen:on_nick(Old, N, S);
		_ -> ok
	end,
	ok;

handle_irc(join, {#user{nick=N}, Channel}, S=#state{modules=M}) ->
	case sets:is_element(z_message, M) of
		true ->	z_message:check_messages_for(N, z_message:get_data(S));
		_ -> ok
	end,
	case sets:is_element(z_seen, M) of
		true -> z_seen:on_join(N, Channel, S);
		_ -> ok
	end,
	ok;

handle_irc(kick, {#user{nick=N}, WhoKicked, Channel, Reason}, S=#state{modules=M}) -> 
	case sets:is_element(z_seen, M) of
		true -> z_seen:on_kick(WhoKicked, Channel, Reason, N, S);
		_ -> ok
	end,
	ok;

handle_irc(topic, _, _) -> ok;
handle_irc(notice, _, _) -> ok;

handle_irc(part, {#user{nick=N}, Channel, Reason}, S=#state{modules=M}) ->
	case sets:is_element(z_seen, M) of
		true -> z_seen:on_part(N, Channel, Reason, S);
		_ -> ok
	end,
	ok;

handle_irc(quit, {#user{nick=N}, Reason}, S=#state{modules=M}) ->
	case sets:is_element(z_seen, M) of
		true -> z_seen:on_quit(N, Reason, S);
		_ -> ok
	end,
	ok;

handle_irc(mode, _, _) -> ok;
handle_irc(numeric, {{rpl,away},_}, _) -> ok;
handle_irc(numeric, {{A,B},Params}, _) -> common:debug("BOT", "Numeric received: ~p_~p ~s", [A,B,string:join(Params," ")]);

handle_irc(Type, Params, _State) -> common:debug("BOT", "unknown irctype ~p <<<~p>>>, continuing", [Type, Params]).

handle_host_command(Rank, Origin, ReplyTo, Ping, Cmd, Params, State=#state{}) ->
	case string:to_lower(Cmd) of
		"update" ->		update;
		"help" ->		core ! {irc, {msg, {ReplyTo, ["builtin host commands: update, reload_all, drop_all, load_mod, drop_mod, reload_mod"]}}},
					handle_command(Rank, Origin, ReplyTo, Ping, Cmd, Params, State);

		"reload_all" ->
			Modules = sets:to_list(State#state.modules),
			self() ! {state, reload_modules(Modules, State)},
			{irc, {msg, {ReplyTo, [Ping, "Reloaded."]}}};

		"drop_all" ->
			Modules = sets:to_list(State#state.modules),
			self() ! {state, unload_modules(Modules, State)},
			{irc, {msg, {ReplyTo, [Ping, "Unloaded."]}}};

		"load_mod" ->
			case Params of
				[] -> {irc, {msg, {ReplyTo, [Ping, "Provide a module to load."]}}};
				ModuleStrings ->
					Modules = lists:map(fun erlang:list_to_atom/1, ModuleStrings),
					self() ! {state, load_modules(Modules, State)},
					{irc, {msg, {ReplyTo, [Ping, "Loaded."]}}}
			end;

		"drop_mod" ->
			case Params of
				[] -> {irc, {msg, {ReplyTo, [Ping, "Provide a module to unload."]}}};
				ModuleStrings ->
					Modules = lists:map(fun erlang:list_to_atom/1, ModuleStrings),
					self() ! {state, unload_modules(Modules, State)},
					{irc, {msg, {ReplyTo, [Ping, "Unloaded."]}}}
			end;

		"reload_mod" ->
			case Params of
				[] -> {irc, {msg, {ReplyTo, [Ping, "Provide a module to reload."]}}};
				ModuleStrings ->
					Modules = lists:map(fun erlang:list_to_atom/1, ModuleStrings),
					self() ! {state, reload_modules(Modules, State)},
					{irc, {msg, {ReplyTo, [Ping, "Reloaded."]}}}
			end;

		"recompile_mod" ->
			case Params of
				[] -> {irc, {msg, {ReplyTo, [Ping, "Provide a module to recompile."]}}};
				ModuleStrings ->
					Modules = lists:map(fun erlang:list_to_atom/1, ModuleStrings),
					self () ! {state, recompile_modules(Modules, State)},
					{irc, {msg, {ReplyTo, [Ping, "Done."]}}}
			end;

		_ -> handle_command(Rank, Origin, ReplyTo, Ping, Cmd, Params, State)
	end.

handle_command(Ranks, Origin, ReplyTo, Ping, Cmd, Params, State=#state{commands=Commands}) ->
	Result = lists:foldl(fun
			(Rank, unhandled) ->
				RankCmds = case orddict:find(Rank, Commands) of
					{ok, X} -> X;
					error -> orddict:new()
				end,
				case string:to_lower(Cmd) of
					"help" -> core ! {irc, {msg, {ReplyTo, [io_lib:format("~s commands: ",[Rank]), string:join(orddict:fetch_keys(RankCmds), ", "), "."]}}}, unhandled;
		
					T -> case orddict:find(T, RankCmds) of
						{ok, {_,Result}} -> apply(Result, [Origin, ReplyTo, Ping, Params, State]);
						error -> unhandled
					end
				end;
			(_, Result) -> Result
		end, unhandled, Ranks),
	case string:to_lower(Cmd) of
		"help" -> ok;
		_ ->
			case Result of
				unhandled -> {irc, {msg, {ReplyTo, [Ping, "Unknown command '",Cmd,"'!"]}}};
				_ -> Result
			end
	end.

load_modules(Modules, State) -> lists:foldl(fun load_module/2, State, Modules).

load_module(Module, State) ->
	common:debug("MODULE", "loading ~p", [Module]),

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
	end, State#state.commands, apply(Module, get_commands, [])),

	% Initialise
	apply(Module, initialise, [State#state{commands = NewCmds, modules = sets:add_element(Module, State#state.modules)}]).

unload_modules(Modules, State) -> lists:foldl(fun unload_module/2, State, Modules).

unload_module(Module, State) ->
	common:debug("MODULE", "unloading ~p", [Module]),

	% Remove commands
	Cleaned = orddict:map(fun(_,RankCmds) ->
			orddict:filter(fun(_,{Mod,_}) -> Mod /= Module end, RankCmds)
		end, State#state.commands),
	Removed = orddict:filter(fun(_,V) -> V /= [] end, Cleaned),

%	{Admin,User} = State#state.commands,
%	A = orddict:filter(fun(_, {Mod, _}) -> Mod /= Module end, Admin),
%	B = orddict:filter(fun(_, {Mod, _}) -> Mod /= Module end, User),

	% Deinitialise
	apply(Module, deinitialise, [State#state{commands = Removed, modules = sets:del_element(Module, State#state.modules)}]).

reload_modules(Modules, State) -> lists:foldl(fun reload_module/2, State, Modules).
reload_module(Module, State) -> load_module(Module, unload_module(Module, State)).

recompile_modules(Modules, State) -> lists:foldl(fun recompile_module/2, State, Modules).
recompile_module(Module, State) ->
	try
		Sa = unload_module(Module, State),
		code:purge(Module),
		compile:file(Module),
		code:load_file(Module),
		load_module(Module, Sa)
	catch
		throw:X -> common:debug("MODULE", "Recompile of ~p threw ~p", [Module, X]), State;
		error:X -> common:debug("MODULE", "Recompile of ~p errored ~p", [Module, X]), State;
		exit:X -> common:debug("MODULE", "Recompile of ~p exited ~p", [Module, X]), State
	end.
