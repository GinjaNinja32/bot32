-module(bot).
-compile(export_all).
-compile({no_auto_import,[load_module/2]}).

-include("definitions.hrl").

-record(config, {nick, prefix, permissions, unused=false, user, mode, real, channels, on_join, modules, pass}).

waitfor(Ident) ->
	case whereis(Ident) of
		undefined ->
			timer:sleep(100),
			waitfor(Ident);
		_ -> ok
	end.

init() ->
	code:add_path("./mod/bin"),
	register(bot, self()),
	{SeedA,SeedB,SeedC}=now(),
	random:seed(SeedA,SeedB,SeedC),
	BasePerms = case file:consult("permissions.crl") of
		{ok, [Perms]} -> Perms;
		_ -> orddict:new()
	end,
	BaseConfig = #config{nick="Bot32", prefix=$!, permissions=BasePerms, user="Bot32", mode="0", real="Bot32", channels=sets:new(), on_join=[], modules=[basic], pass=none},
	case file:consult("bot_config.crl") of
		{error, Reason} ->
			logging:log(error, "BOT", "Failed to load config file: ~p.", [Reason]),
			UseConfig = BaseConfig;
		{ok, Terms} ->
			UseConfig = lists:foldl(fun(Option, Config=#config{permissions=Perms, on_join=OJ, channels=C, modules=M}) ->
					case Option of
						{nick, Nick}		when is_list(Nick) orelse is_binary(Nick)	-> Config#config{nick=Nick};
						{user, User}		when is_list(User) orelse is_binary(User)	-> Config#config{user=User};
						{mode, Mode}		when is_list(Mode) orelse is_binary(Mode)	-> Config#config{mode=Mode};
						{real, Real}		when is_list(Real) orelse is_binary(Real)	-> Config#config{real=Real};

						{prefix, Prefix}	when is_integer(Prefix)				-> Config#config{prefix=[Prefix]};
						{prefixes, Prefixes}	when is_list(Prefixes)				-> Config#config{prefix=Prefixes};

						{permission, Who, P}	when is_atom(P) ->
														NewPerms = case orddict:find(Who, Perms) of
															{ok, V} ->
																case lists:member(P, V) of
																	true -> V;
																	false -> [P | V]
																end;
															error -> [user, P]
														end,
														Config#config{permissions=orddict:store(Who, NewPerms, Perms)};

						{channel, Channel}	when is_list(Channel) orelse is_binary(Channel)	-> Config#config{channels=sets:add_element(Channel, C)};
						{channels, Channels}	when is_list(Channels)				-> Config#config{channels=lists:foldl(fun sets:add_element/2, C, Channels)};

						{module, Mod}		when is_atom(Mod)				-> Config#config{modules = [Mod | M]};
						{modules, Mods}		when is_list(Mods)				-> Config#config{modules=lists:foldl(
																fun	(Mod, MX) when is_atom(Mod) -> [Mod|MX];
																	(Mod, _) -> logging:log(error, "BOT", "Non-atomic module ~p specified!", [Mod])
																 end, M, Mods)};

						{pass, Password}	when Password == none orelse is_list(Password)  -> Config#config{pass=Password};
						{on_join, Cmd}		when is_tuple(Cmd)				-> Config#config{on_join = [Cmd | OJ]};

						T -> logging:log(error, "BOT", "Failed to parse config line ~p!", [T]), Config
					end
				end, BaseConfig, Terms)
	end,
	waitfor(core), % wait for core to startup
	if
		UseConfig#config.pass /= none -> core ! {irc, {pass, UseConfig#config.pass}};
		true -> ok
	end,
	core ! {irc, {user, {UseConfig#config.user, UseConfig#config.mode, UseConfig#config.real}}},
	core ! {irc, {nick, UseConfig#config.nick}},
	timer:sleep(250),
	lists:foreach(fun(T) ->
			core ! {irc, T}
		end, UseConfig#config.on_join),
	timer:sleep(50), % wait for server auth
	lists:foreach(fun(T) -> core ! {irc, {join, T}} end, sets:to_list(UseConfig#config.channels)),
	CallWhoWhat = case file:consult("call.crl") of
		{ok, [Call]} -> Call;
		_ -> []
	end,
	logging:log(info, "BOT", "starting"),
	State = load_modules(UseConfig#config.modules,
			    #state{
				nick     = UseConfig#config.nick,
				prefix   = UseConfig#config.prefix,
				permissions = UseConfig#config.permissions,
				aliases = orddict:new(),
				commands = orddict:new(),
				moduledata  = [{callme, CallWhoWhat}],
				modules = sets:new()
			    }),
	case loop(State) of
		FinalState=#state{} ->
			X = file:write_file("permissions.crl", io_lib:format("~p.~n", [FinalState#state.permissions])),
			logging:log(info, "BOT", "permissions save: ~p", [X]),
			Y = file:write_file("call.crl", io_lib:format("~p.~n", [case orddict:find(callme, FinalState#state.moduledata) of {ok,Z}->Z; error->[] end])),
			logging:log(info, "BOT", "call save: ~p", [Y]),
			lists:foreach(fun(Module) ->
					lists:member({deinitialise,1}, Module:module_info(exports)) andalso apply(Module, deinitialise, [FinalState])
				end, sets:to_list(FinalState#state.modules)),
			logging:log(info, "BOT", "quitting");
		T -> logging:log(info, "BOT", "quitting under condition ~p", [T])
	end.

reinit(State) ->
	register(bot, self()),
	logging:log(info, "BOT", "starting"),
	case loop(State) of
		FinalState=#state{} ->
			lists:foreach(fun(Module) ->
					lists:member({deinitialise,1}, Module:module_info(exports)) andalso apply(Module, deinitialise, [FinalState])
				end, sets:to_list(FinalState#state.modules)),
			logging:log(info, "BOT", "quitting");
		T -> logging:log(info, "BOT", "quitting under condition ~p", [T])
	end.

notify_error(msg, {#user{nick=N}, MyNick, _}, #state{nick=MyNick}) -> {irc, {msg, {N, "Error!"}}};
notify_error(msg, {#user{nick=N}, Channel, _}, _) -> {irc, {msg, {Channel, [N, ": Error!"]}}};
notify_error(X, Y, _) -> logging:log(error, "BOT", "~p : ~p", [X,Y]).

loop(State = #state{}) ->
	case receive
		{ircfwd, T} -> {irc, T};
		{irc, {Type, Params}} ->
			case catch handle_irc(Type, Params, State) of
				{'EXIT', {Reason, Stack}} -> logging:log(error, "BOT", "handle_irc errored ~p (~p), continuing", [Reason, Stack]), notify_error(Type, Params, State);
				{'EXIT', Term} ->  logging:log(error, "BOT", "handle_irc exited ~p, continuing",  [Term]), notify_error(Type, Params, State);
				T -> T
			end;
		T when is_atom(T) -> T;
		{T, K} when is_atom(T) -> {T, K};
		T -> logging:log(error, "BOT", "unknown receive ~p, continuing", [T])
	end of
		{multi, List} -> lists:foreach(fun(T) -> core ! T end, List), bot:loop(State);
		{irc, What} -> core ! {irc,What}, bot:loop(State);
		quit -> State;
		error -> error;
		ok -> bot:loop(State);
		{state, S = #state{}} -> bot:loop(S);
		{setkey, {Key, Val}} -> bot:loop(State#state{moduledata = orddict:store(Key, Val, State#state.moduledata)});
		update ->
			spawn(common,purge_call,[bot,reinit,State]),
			ok;
		{update,Chan} ->
			spawn(common,purge_call_report,[bot,reinit,State,Chan]),
			ok;
		S -> logging:log(error, "BOT", "unknown code ~p, continuing", [S]), bot:loop(State)
	end.

message_admins(Category, Msg, Admins) ->
	logging:log(info, "ADMIN", [Category, ": ", Msg]),
	lists:foreach(fun(T) ->
			core ! {irc, {msg, {T, [Category, ": ", Msg]}}}
		end, sets:to_list(Admins)),
	ok.

message_all_rank(Category, Msg, Rank, Permissions) ->
	logging:log(info, Rank, "~s: ~s", [Category, Msg]),
	lists:foreach(fun({N,_U,_H}) ->
			core ! {irc, {msg, {N, [Category, ": ", Msg]}}}
		end, orddict:fetch_keys(orddict:filter(fun(_,V) -> lists:member(Rank,V) end, Permissions))).

rankof(Usr=#user{}, Permissions) -> rankof(Usr, Permissions, none).
rankof(#user{nick=N,username=U,host=H}, Permissions, Channel) ->
	C = case Channel of
		none ->	none;
		_ -> list_to_binary(Channel)
	end,
	orddict:fold(fun
			({Nick,User,Host}, Perms, PermsSoFar) ->
				case    (re:run(N, util:regex_star(Nick), [{capture, none}, caseless]) == match)
				andalso (re:run(U, util:regex_star(User), [{capture, none}]) == match)
				andalso (re:run(H, util:regex_star(Host), [{capture, none}]) == match) of
					true -> lists:umerge(lists:usort(Perms), PermsSoFar);
					false -> PermsSoFar
				end;
			(Chan, Perms, PermsSoFar) ->
				if Chan == C -> lists:umerge(lists:usort(Perms), PermsSoFar);
				   true -> PermsSoFar
				end
		end, [user], Permissions).

rankof_chan(Channel, Permissions) ->
	case orddict:find(list_to_binary(Channel), Permissions) of
		{ok, Value} -> Value;
		error -> [user]
	end.

hasperm(_, user, _) -> true;
hasperm(User=#user{}, Perm, Permissions) ->
	lists:member(Perm, rankof(User, Permissions)).

parse_command([],_,_,_) -> notcommand;
parse_command(Params, Prefix, BotAliases, IsQuery) when is_list(Prefix) ->
	<<FirstChar/utf8, Rest/binary>> = list_to_binary(hd(Params)),
	case lists:member(FirstChar, Prefix) of
		true when Rest /= <<>> -> {binary_to_list(Rest), tl(Params)};
		true -> notcommand;
		false -> parse_command(Params, none, BotAliases, IsQuery)
	end;
parse_command(Params, Prefix, BotAliases, IsQuery) ->
	case hd(Params) of
		[Prefix | Command] -> {Command, tl(Params)};
		X when length(Params) > 1 ->
			case lists:any(fun(Alias) -> R=util:regex_escape(Alias), re:run(X, <<"^", R/binary, "($|[^a-zA-Z0-9])">>, [caseless, {capture, none}]) == match end, BotAliases) of
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
		X when IsQuery -> {X,[]};
		_ -> notcommand
	end.

check_utf8(<<>>) -> true;
check_utf8(<<_/utf8,B/binary>>) -> check_utf8(B);
check_utf8(X) -> io:fwrite("~p~n", [X]), false.

%handle_irc(msg, {_,T,_}, _) when T /= "#bot32-test" -> ok;
handle_irc(msg, Params={User=#user{nick=Nick}, Channel, Tokens}, OState=#state{nick=MyNick, prefix=Prefix, permissions=Permissions, modules=M}) ->
	State = case sets:is_element(seen, M) of
		true -> if
				Channel /= MyNick -> seen:privmsg_hook(Nick, Channel, OState);
				true -> OState
			end;
		_ -> OState
	end,
	case lists:all(fun(T) -> check_utf8(list_to_binary(T)) end, Tokens) of
		false -> logging:log(utf8, "BOT", "Ignoring '~s' due to invalid UTF-8", [string:join(Tokens, " ")]);
		true ->
	case hasperm(User, ignore, Permissions) of
		true ->
			logging:log(ignore, "BOT", "Ignoring ~s!~s@~s: ~s.", [Nick, User#user.username, User#user.host, string:join(Tokens, " ")]),
			ok;
		false ->
			case Channel of
				MyNick ->
					ReplyChannel = Nick,
					ReplyPing = "",
					case hasperm(User, admin, Permissions) of
						true -> ok;
						_ -> message_all_rank(["Query from ",Nick], string:join(Tokens, " "), pmlog, Permissions)
					end;
				_ ->
					ReplyChannel = Channel,
					ReplyPing = case orddict:find(callme, State#state.moduledata) of
						{ok, Dict} ->
							case orddict:find(string:to_lower(Nick), Dict) of
								{ok, V} -> V ++ ": ";
								error -> Nick ++ ": "
							end;
						error -> Nick ++ ": "
					end
			end,
			logging:log(debug2, "BOT", "Parsing command: ~p", [Tokens]),
			case parse_command(Tokens, Prefix, [MyNick, "NT"], Channel == MyNick) of
				{RCommand, RArguments} ->
					logging:log(info, "BOT", "Command in ~s from ~s: ~s ~s", [Channel, User#user.nick, RCommand, string:join(RArguments, " ")]),
					{Command, Arguments} = decode_alias(RCommand, State#state.aliases, RArguments),
					Rank = rankof(User, Permissions, ReplyChannel),
					case hasperm(User, host, Permissions) of
						true -> handle_host_command(Rank, User, ReplyChannel, ReplyPing, Command, Arguments, State);
						false ->     handle_command(Rank, User, ReplyChannel, ReplyPing, Command, Arguments, State)
					end;
				notcommand ->
					NewState = lists:foldl(fun(Module, CState) ->
							call_or(Module, handle_event, [msg, Params, CState], null),
							call_or(Module, do_extras, [Tokens, ReplyChannel, ReplyPing], null),
							call_or(Module, handle_event_s, [msg, Params, CState], CState)
						end, State, sets:to_list(State#state.modules)),
					{state, NewState}
			end
	end
	end;

handle_irc(ctcp, {Type, #user{nick=Nick}, _Message}, _State) ->
	case Type of
		version -> {irc, {ctcp_re, {version, Nick, ?VERSION}}};
		action -> ok;
		_ -> logging:log(error, "BOT", "Unknown CTCP message ~p, continuing", [Type])
	end;

handle_irc(nick, {#user{nick=MyNick}, NewNick}, State=#state{nick=MyNick}) -> {state, State#state{nick=NewNick}};

handle_irc(notice, _, _) -> ok;

handle_irc(numeric, {{rpl,away},_}, _) -> ok;
handle_irc(numeric, {{A,B},Params}, _) -> logging:log(info, "BOT", "Numeric received: ~p_~p ~s", [A,B,string:join(Params," ")]);

handle_irc(Type, Params, State) ->
	NewState = lists:foldl(fun(Module, CState) ->
				call_or(Module, handle_event, [Type, Params, CState], null),
				call_or(Module, handle_event_s, [Type, Params, CState], CState)
			end, State, sets:to_list(State#state.modules)),
	{state, NewState}.

handle_host_command(Rank, User, ReplyTo, Ping, Cmd, Params, State=#state{}) ->
	case string:to_lower(Cmd) of
		"update" ->		{update, ReplyTo};
		"help" ->	if Params == [] ->
						core ! {irc, {msg, {User#user.nick, ["builtin host commands: update, reload_all, drop_all, load_mod, drop_mod, reload_mod"]}}};
						true -> ok
					end,
					handle_command(Rank, User, ReplyTo, Ping, Cmd, Params, State);

		"modules" ->
			{irc, {msg, {ReplyTo, [Ping, string:join(lists:map(fun atom_to_list/1, lists:sort(sets:to_list(State#state.modules))), " ")]}}};

		"reload_all" ->
			Modules = sets:to_list(State#state.modules),
			core ! {irc, {msg, {ReplyTo, [Ping, "Reloaded."]}}},
			{state, reload_modules(Modules, State)};

		"drop_all" ->
			Modules = sets:to_list(State#state.modules),
			core ! {irc, {msg, {ReplyTo, [Ping, "Unloaded."]}}},
			{state, unload_modules(Modules, State)};

		"load" ->
			case Params of
				[] -> {irc, {msg, {ReplyTo, [Ping, "Provide a module to load."]}}};
				ModuleStrings ->
					Modules = lists:map(fun erlang:list_to_atom/1, ModuleStrings),
					core ! {irc, {msg, {ReplyTo, [Ping, "Loaded."]}}},
					{state, load_modules(Modules, State)}
			end;

		"drop" ->
			case Params of
				[] -> {irc, {msg, {ReplyTo, [Ping, "Provide a module to unload."]}}};
				ModuleStrings ->
					Modules = lists:map(fun erlang:list_to_atom/1, ModuleStrings),
					core ! {irc, {msg, {ReplyTo, [Ping, "Unloaded."]}}},
					{state, unload_modules(Modules, State)}
			end;

		"reload" ->
			case Params of
				[] -> {irc, {msg, {ReplyTo, [Ping, "Provide a module to reload."]}}};
				ModuleStrings ->
					Modules = lists:map(fun erlang:list_to_atom/1, ModuleStrings),
					core ! {irc, {msg, {ReplyTo, [Ping, "Reloaded."]}}},
					{state, reload_modules(Modules, State)}
			end;

		"recompile" ->
			case Params of
				[] -> {irc, {msg, {ReplyTo, [Ping, "Provide a module to recompile."]}}};
				ModuleStrings ->
					Modules = lists:map(fun erlang:list_to_atom/1, ModuleStrings),
					core ! {irc, {msg, {ReplyTo, [Ping, "Done."]}}},
					{state, recompile_modules(Modules, State)}
			end;

		"isalias" ->
			case Params of
				[New] ->
					Msg = case orddict:find(New, State#state.aliases) of
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
					{NewA, Msg} = case orddict:find(New, State#state.aliases) of
						{ok, _} -> {orddict:erase(New, State#state.aliases), "Removed."};
						error -> {State#state.aliases, "Could not find alias."}
					end,
					core ! {irc, {msg, {ReplyTo, [Ping, Msg]}}},
					{state, State#state{aliases=NewA}};
				_ -> {irc, {msg, {ReplyTo, [Ping, "Provide an alias to clear!"]}}}
			end;

		"alias" ->
			case Params of
%				[New, Real, "*"] ->
%					core ! {irc, {msg, {ReplyTo, [Ping, "Done."]}}},
%					{state, State#state{aliases=orddict:store(New, Real, State#state.aliases)}};
				[New, Real | ArgSpec] ->
					case parse_argspec(ArgSpec) of
						{ok,Spec} ->
							core ! {irc, {msg, {ReplyTo, [Ping, "Done."]}}},
							{state, State#state{aliases=orddict:store(New, {Real, Spec}, State#state.aliases)}};
						error -> {irc, {msg, {ReplyTo, [Ping, "Illegal argspec."]}}}
					end;
				_ -> {irc, {msg, {ReplyTo, [Ping, "Provide a command and an alias!"]}}}
			end;

		_ -> handle_command(Rank, User, ReplyTo, Ping, Cmd, Params, State)
	end.

handle_command(Ranks, User, ReplyTo, Ping, Cmd, Params, State=#state{commands=Commands}) ->
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
							OldDict = case orddict:find(callme, State#state.moduledata) of
								{ok, V} -> V;
								error -> orddict:new()
							end,
							NewDict = orddict:store(Usr, string:join(Nick, " "), OldDict),
							core ! {irc, {msg, {ReplyTo, [Ping, "Done."]}}},
							Y = file:write_file("call.crl", io_lib:format("~p.~n", [NewDict])),
							logging:log(info, "BOT", "call save: ~p", [Y]),
							{state, State#state{moduledata=orddict:store(callme, NewDict, State#state.moduledata)}}
					end
			end;
		"help" when Params /= [] ->
			case orddict:find(hd(Params), State#state.aliases) of
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
								{ok, {Mod,Result}} ->
									UseOrigin = case call_or(Mod, origin_mode, [], basic) of
										basic -> User#user.nick;
										full -> User
									end,
									apply(Result, [UseOrigin, ReplyTo, Ping, Params, State]);
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
					case alternate_commands([Cmd | Params], State#state.modules) of
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
		end, [fun select_or_string/1, fun alternate_eightball/1], sets:to_list(Modules)),
	lists:foldl(fun
			(Func, false) -> Func(Tokens);
			(_,Re) -> Re
		end, false, AltFunctions).

select_or_string(List) ->
	case collapse_or_string(List, [], []) of
		false -> false;
		[] -> false;
		[_] -> false;
		Options -> lists:nth(random:uniform(length(Options)), Options)
	end.

collapse_or_string([], [], _) -> false;
collapse_or_string([], COpt, Options) -> [COpt | Options];
collapse_or_string(["or"|_], [], _) -> false;
collapse_or_string(["or"|L], COpt, Options) -> collapse_or_string(L, [], [COpt | Options]);
collapse_or_string([T|L], [], Options) -> collapse_or_string(L, [T], Options);
collapse_or_string([T|L], COpt, Options) -> collapse_or_string(L, [COpt,32|T], Options).

alternate_eightball(List) ->
	case util:lasttail(util:lasttail(List)) of
		$? -> util:eightball();
		_ -> false
	end.

load_modules(Modules, State) -> lists:foldl(fun load_module/2, State, Modules).

load_module(Module, State) ->
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
	end, State#state.commands, call_or(Module, get_commands, [], [])),

	% Load aliases
	NewAliases = lists:foldl(
		fun
			({Real, Aliases}, AllAliases) ->
				lists:foldl(fun(A, Al) -> orddict:store(A,Real,Al) end, AllAliases, Aliases)
		end, State#state.aliases, call_or(Module, get_aliases, [], [])),

	% Initialise
	case call_or(Module, data_persistence, [], manual) of
		manual ->
			BaseState = State#state{commands = NewCmds, aliases = NewAliases, modules = sets:add_element(Module, State#state.modules)},
			call_or(Module, initialise, [BaseState], BaseState);
		automatic ->
			Data = modload_auto(Module),
			State#state{commands = NewCmds, aliases = NewAliases, modules = sets:add_element(Module, State#state.modules), moduledata = orddict:store(Module, Data, State#state.moduledata)};
		none ->
			State#state{commands = NewCmds, aliases = NewAliases, modules = sets:add_element(Module, State#state.modules)}
	end.

call_or(Mod, Func, Args, Or) ->
	case lists:member({Func,length(Args)}, Mod:module_info(exports)) of
		true -> apply(Mod,Func,Args);
		false -> Or
	end.

modload_auto(Module) ->
	case file:consult(["modules/", Module, ".crl"]) of
		{ok, [Data]} -> logging:log(info, Module, "Loaded."), Data;
		{ok, _} -> logging:log(error, Module, "Incorrect format."), Module:default_data();
		{error, T} -> logging:log(error, Module, "Error loading: ~p", [T]), Module:default_data()
	end.


unload_modules(Modules, State) -> lists:foldl(fun unload_module/2, State, Modules).

unload_module(Module, State) ->
	case sets:is_element(Module, State#state.modules) of
		false -> State;
		true ->
	logging:log(info, "MODULE", "unloading ~p", [Module]),

	% Remove commands
	Cleaned = orddict:map(fun(_,RankCmds) ->
			orddict:filter(fun(_,{Mod,_}) -> Mod /= Module end, RankCmds)
		end, State#state.commands),
	Removed = orddict:filter(fun(_,V) -> V /= [] end, Cleaned),

	% Remove aliases
	NewAliases = lists:foldl(fun({Real,Aliases}, AllAliases) ->
			orddict:filter(fun(K,V) -> not lists:member(K, Aliases) andalso V /= Real end, AllAliases)
		end, State#state.aliases, call_or(Module, get_aliases, [], [])),

	% Deinitialise
	case case lists:member({data_persistence,0}, Module:module_info(exports)) of
		true -> Module:data_persistence();
		false -> manual
	end of
		manual ->
			BaseState = State#state{commands = Removed, aliases=NewAliases, modules = sets:del_element(Module, State#state.modules)},
			call_or(Module, deinitialise, [BaseState], BaseState);
		automatic ->
			case orddict:find(Module, State#state.moduledata) of
				{ok, V} ->
					Status = file:write_file(["modules/", Module, ".crl"], io_lib:format("~p.~n", [V])),
					logging:log(info, Module, "Save status: ~p", [Status]);
				error ->
					file:delete(["modules/", Module, ".crl"]),
					logging:log(info, Module, "Save found no data.")
			end,
			State#state{commands = Removed, aliases=NewAliases, modules=sets:del_element(Module, State#state.modules), moduledata=orddict:erase(Module, State#state.moduledata)};
		none ->
			State#state{commands = Removed, aliases=NewAliases, modules=sets:del_element(Module, State#state.modules), moduledata=orddict:erase(Module, State#state.moduledata)}
	end
	end.

reload_modules(Modules, State) -> lists:foldl(fun reload_module/2, State, Modules).
reload_module(Module, State) -> load_module(Module, unload_module(Module, State)).

recompile_modules(Modules, State) -> lists:foldl(fun recompile_module/2, State, Modules).
recompile_module(Module, State) ->
	try
		Sa = unload_module(Module, State),
		io:fwrite("purge ~p~n", [code:purge(Module)]),
		io:fwrite("compile ~p~n", [compile:file("mod/" ++ atom_to_list(Module), [report_errors, report_warnings, {outdir, "./mod/bin/"}])]),
		io:fwrite("load ~p~n", [code:load_file(Module)]),
		load_module(Module, Sa)
	catch
		throw:X -> logging:log(error, "MODULE", "Recompile of ~p threw ~p", [Module, X]), State;
		error:X -> logging:log(error, "MODULE", "Recompile of ~p errored ~p", [Module, X]), State;
		exit:X -> logging:log(error, "MODULE", "Recompile of ~p exited ~p", [Module, X]), State
	end.

decode_alias(Command, Aliases, Arguments) ->
	case orddict:find(Command, Aliases) of
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
	
