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
	register(bot, self()),
	{SeedA,SeedB,SeedC}=now(),
	random:seed(SeedA,SeedB,SeedC),
	BasePerms = case file:consult("permissions.crl") of
		{ok, [Perms]} -> Perms;
		_ -> orddict:new()
	end,
	BaseConfig = #config{nick="Bot32", prefix=$!, permissions=BasePerms, user="Bot32", mode="0", real="Bot32", channels=sets:new(), on_join=[], modules=[z_basic], pass=none},
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

parse_command([],_,_) -> notcommand;
parse_command(Params, Prefix, BotAliases) when is_list(Prefix) ->
	<<FirstChar/utf8, Rest/binary>> = list_to_binary(hd(Params)),
	case lists:member(FirstChar, Prefix) of
		true when Rest /= <<>> -> {binary_to_list(Rest), tl(Params)};
		true -> notcommand;
		false -> parse_command(Params, none, BotAliases)
	end;
parse_command(Params, Prefix, BotAliases) ->
	case hd(Params) of
		[Prefix | Command] -> {Command, tl(Params)};
		X when length(Params) > 1 ->
			case lists:any(fun(Alias) -> R=util:regex_escape(Alias), re:run(X, <<"^", R/binary, "($|[^a-zA-Z0-9])">>, [caseless, {capture, none}]) == match end, BotAliases) of
				true -> case tl(Params) of
						[] -> {[], []};
						_ -> {hd(tl(Params)), tl(tl(Params))}
					end;
				false -> notcommand
			end;
		_ -> notcommand
	end.

%handle_irc(msg, {_,T,_}, _) when T /= "#bot32-test" -> ok;
handle_irc(msg, Params={User=#user{nick=Nick}, Channel, Tokens}, State=#state{nick=MyNick, prefix=Prefix, permissions=Permissions, modules=M}) ->
	case sets:is_element(z_seen, M) of
		true -> if
				Channel /= MyNick -> z_seen:on_privmsg(Nick, Channel, State);
				true -> ok
			end;
		_ -> ok
	end,
	case hasperm(User, ignore, Permissions) of
		true -> ok;
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
			case parse_command(de_russian(Tokens), Prefix, [MyNick, "NT"]) of
				{RCommand, RArguments} ->
					logging:log(info, "BOT", "Command in ~s from ~s: ~s ~s", [Channel, User#user.nick, RCommand, string:join(RArguments, " ")]),
					{Command, Arguments} = decode_alias(RCommand, State#state.aliases, RArguments),
					Rank = rankof(User, Permissions, ReplyChannel),
					case hasperm(User, host, Permissions) of
						true -> handle_host_command(Rank, Nick, ReplyChannel, ReplyPing, Command, Arguments, State);
						false ->     handle_command(Rank, Nick, ReplyChannel, ReplyPing, Command, Arguments, State)
					end;
				notcommand ->
					case lists:dropwhile(fun(X) -> re:run(X, "^https?://.*$", [{capture, none}]) /= match end, Tokens) of
						[] -> ok;
						[URL|_] -> showurl(ReplyChannel, ReplyPing, URL, "~s")
					end,
					case re:run(string:join(Tokens, " "), "\\[\\[([^ ][^\]]+[^ ])\\]\\]", [{capture, all_but_first, binary}]) of
						{match, [Page]} ->
							UR = "http://wiki.baystation12.net/" ++ re:replace(Page, " ", "_", [{return, list}, global]),
							showurl(ReplyChannel, ReplyPing, UR, UR ++ " - ~s", "Page not found!");
						_ -> ok
					end,
					do_pr_linking(Tokens, ReplyChannel, ReplyPing),
					do_russian(Tokens, ReplyChannel, ReplyPing),
					lists:foreach(fun(Module) ->
							lists:member({handle_event,3}, Module:module_info(exports))
							andalso Module:handle_event(msg, Params, State)
						end, sets:to_list(State#state.modules))
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
	lists:foreach(fun(Module) ->
				lists:member({handle_event,3}, Module:module_info(exports))
				andalso Module:handle_event(Type, Params, State)
			end, sets:to_list(State#state.modules)).

handle_host_command(Rank, Origin, ReplyTo, Ping, Cmd, Params, State=#state{}) ->
	case string:to_lower(Cmd) of
		"update" ->		{update, ReplyTo};
		"help" ->		core ! {irc, {msg, {Origin, ["builtin host commands: update, reload_all, drop_all, load_mod, drop_mod, reload_mod"]}}},
					handle_command(Rank, Origin, ReplyTo, Ping, Cmd, Params, State);

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

		"load_mod" ->
			case Params of
				[] -> {irc, {msg, {ReplyTo, [Ping, "Provide a module to load."]}}};
				ModuleStrings ->
					Modules = lists:map(fun erlang:list_to_atom/1, ModuleStrings),
					core ! {irc, {msg, {ReplyTo, [Ping, "Loaded."]}}},
					{state, load_modules(Modules, State)}
			end;

		"drop_mod" ->
			case Params of
				[] -> {irc, {msg, {ReplyTo, [Ping, "Provide a module to unload."]}}};
				ModuleStrings ->
					Modules = lists:map(fun erlang:list_to_atom/1, ModuleStrings),
					core ! {irc, {msg, {ReplyTo, [Ping, "Unloaded."]}}},
					{state, unload_modules(Modules, State)}
			end;

		"reload_mod" ->
			case Params of
				[] -> {irc, {msg, {ReplyTo, [Ping, "Provide a module to reload."]}}};
				ModuleStrings ->
					Modules = lists:map(fun erlang:list_to_atom/1, ModuleStrings),
					core ! {irc, {msg, {ReplyTo, [Ping, "Reloaded."]}}},
					{state, reload_modules(Modules, State)}
			end;

		"recompile_mod" ->
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

		_ -> handle_command(Rank, Origin, ReplyTo, Ping, Cmd, Params, State)
	end.

handle_command(Ranks, Origin, ReplyTo, Ping, Cmd, Params, State=#state{commands=Commands}) ->
	Result = case string:to_lower(Cmd) of
		"call" ->
			case Params of
				[] -> {irc, {msg, {ReplyTo, [Ping, "Supply either a nick, or the string 'me', and a name to use!"]}}};
				[_] -> {irc, {msg, {ReplyTo, [Ping, "Supply a name to use!"]}}};
				[Usr|Nick] ->
					User = case Usr of
						"me" -> string:to_lower(Origin);
						_ -> string:to_lower(Usr)
					end,
					case case {lists:member(admin, Ranks), string:to_lower(Origin)} of
						{true,_} -> ok;
						{_,User} -> ok;
						_ -> false
					end of
						false -> {irc, {msg, {ReplyTo, [Ping, "You are not authorised to do that!"]}}};
						ok ->
							OldDict = case orddict:find(callme, State#state.moduledata) of
								{ok, V} -> V;
								error -> orddict:new()
							end,
							NewDict = orddict:store(User, string:join(Nick, " "), OldDict),
							core ! {irc, {msg, {ReplyTo, [Ping, "Done."]}}},
							Y = file:write_file("call.crl", io_lib:format("~p.~n", [NewDict])),
							logging:log(info, "BOT", "call save: ~p", [Y]),
							{state, State#state{moduledata=orddict:store(callme, NewDict, State#state.moduledata)}}
					end
			end;
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
								do_help_for(Origin, Rank, orddict:fetch_keys(RankCmds)),
								unhandled;
								%core ! {irc, {msg, {Origin, [io_lib:format("~s commands: ",[Rank]), string:join(orddict:fetch_keys(RankCmds), ", "), "."]}}}, unhandled;
							T -> case orddict:find(T, RankCmds) of
								{ok, {_,Result}} -> apply(Result, [Origin, ReplyTo, Ping, Params, State]);
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

russian_keymap() ->
	[
		{1025,96},{1040,70},{1041,44},{1042,68},{1043,85},{1044,76},
		{1045,84},{1046,58},{1047,80},{1048,66},{1049,81},{1050,82},
		{1051,75},{1052,86},{1053,89},{1054,74},{1055,71},{1056,72},
		{1057,67},{1058,78},{1059,69},{1060,65},{1061,91},{1062,87},
		{1063,88},{1064,73},{1065,79},{1066,93},{1067,83},{1068,77},
		{1069,39},{1070,46},{1071,90},{1072,102},{1073,44},{1074,100},
		{1075,117},{1076,108},{1077,116},{1078,59},{1079,112},{1080,98},
		{1081,113},{1082,114},{1083,107},{1084,118},{1085,121},{1086,106},
		{1087,103},{1088,104},{1089,99},{1090,110},{1091,101},{1092,97},
		{1093,91},{1094,119},{1095,120},{1096,105},{1097,111},{1098,93},
		{1099,115},{1100,109},{1101,39},{1102,46},{1103,122},{1105,96}
	].

do_russian(Tokens, ReplyChannel, ReplyPing) ->
	String = utf8(list_to_binary(string:join(Tokens, " "))),
	case is_russian(String) of
		true -> core ! {irc, {msg, {ReplyChannel, [ReplyPing, "Did you mean: ", convert_russian(String)]}}};
		false -> ok
	end.

de_russian(Tokens) ->
	lists:map(fun(T) ->
			UTFed = utf8(list_to_binary(T)),
			case is_russian(UTFed) of
				true -> convert_russian(UTFed);
				false -> T
			end
		end, Tokens).

is_russian([]) -> false;
is_russian(String) ->
	NumRussian = lists:foldl(fun(S,N) -> case orddict:is_key(S, russian_keymap()) of true -> N+1; false -> N end end, 0, String),
	X = NumRussian >= length(String)/3,
	X.

convert_russian(String) ->
	lists:map(fun(S) ->
			case orddict:find(S, russian_keymap()) of
				{ok, V} -> V;
				error -> S
			end
		end, String).

utf8(B) -> lists:reverse(utf8(B,[])).
utf8(<<>>, L) -> L;
utf8(<<A/utf8, B/binary>>, L) -> utf8(B, [A | L]).

default_user_repo("#yonaguni") -> {"Yonaguni", "Baystation12"};
default_user_repo("#bot32-test") -> {"GinjaNinja32", "bot32"};
default_user_repo(_) -> {"Baystation12", "Baystation12"}.

do_pr_linking(Tokens, Channel, Ping) ->
	lists:foreach(fun(T) -> do_pr_link_token(T, Channel, Ping) end, Tokens).

do_pr_link_token(Token, Channel, Ping) ->
	{DefU, DefR} = default_user_repo(Channel),
	case case re:run(Token, "^(?:([a-zA-Z0-9_\-]+)(?:/([a-zA-Z0-9_\-]+))?)?(?:\\[([0-9]{1,5})\\]|#([0-9]{2,5}))(?:$|[^0-9])", [{capture, all_but_first, list}]) of
		{match, ["", "", "",  N]} -> {DefU, DefR, N};
		{match, [ U, "", "",  N]} -> {   U, DefR, N};
		{match, [ U,  R, "",  N]} -> {   U,    R, N};
		{match, ["", "",      N]} -> {DefU, DefR, N};
		{match, [ U, "",      N]} -> {   U, DefR, N};
		{match, [ U,  R,      N]} -> {   U,    R, N};
		nomatch -> false
	end of
		{User, Repo, Num} ->
			os:putenv("url", ["http://github.com/", User, $/, Repo, "/issues/", Num]),
			URLTitle = string:strip(re:replace(os:cmd("/home/bot32/urltitle.sh $url"), "([^·]*·[^·]*) · .*", "\\1", [{return, list}])),
			case re:run(URLTitle, "Issue #[0-9]+$", [{capture, none}]) of
				match -> ShowURL = ["http://github.com/", User, $/, Repo, "/issues/", Num];
				nomatch -> ShowURL = ["http://github.com/", User, $/, Repo, "/pull/", Num]
			end,
			core ! {irc, {msg, {Channel, [Ping, ShowURL, " - ", util:parse_htmlentities(list_to_binary(URLTitle))]}}};
		false -> ok
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
	end, State#state.commands, Module:get_commands()),

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
	end.

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
		throw:X -> logging:log(error, "MODULE", "Recompile of ~p threw ~p", [Module, X]), State;
		error:X -> logging:log(error, "MODULE", "Recompile of ~p errored ~p", [Module, X]), State;
		exit:X -> logging:log(error, "MODULE", "Recompile of ~p exited ~p", [Module, X]), State
	end.

showurl(Channel, Ping, URL, Format) -> spawn(bot, showurl_raw, [Channel, Ping, URL, Format, false]).
showurl(Channel, Ping, URL, Format, NotFound) -> spawn(bot, showurl_raw, [Channel, Ping, URL, Format, NotFound]).

showurl_raw(Channel, Ping, URL, Format, NotFound) ->
	os:putenv("url", URL),
	case re:replace(os:cmd("/home/bot32/urltitle.sh $url"), "^[ \\t\\n]+(.*[^ \\t\\n])[ \\t\\n]+$", "\\1", [{return, binary}]) of
		<<>> when NotFound /= false -> core ! {irc, {msg, {Channel, [Ping, NotFound]}}};
		<<>> -> ok;
		Sh -> core ! {irc, {msg, {Channel, [Ping, io_lib:format(Format, [util:parse_htmlentities(Sh)])]}}}
	end.

decode_alias(Command, Aliases, Arguments) ->
	case orddict:find(Command, Aliases) of
		{ok, {V,Spec}} -> {V, apply_argspec(Spec, Arguments)};
		{ok, V} -> {V, Arguments};
		error -> {Command, Arguments}
	end.

apply_argspec(Spec, Arguments) ->
	lists:flatmap(fun
			({T}) when is_integer(T) -> lists:nthtail(T-1, Arguments);
			(T) when is_integer(T) -> [lists:nth(T, Arguments)];
			(T) -> [T]
		end, Spec).

parse_argspec(Params) ->
	{ok, lists:map(fun(T) ->
			case re:run(T, <<"(\\\\|\\*)([0-9]+)">>, [{capture, all_but_first, binary}]) of
				{match, [<< "*">>, SNum]} -> {binary_to_integer(SNum)};
				{match, [<<"\\">>, SNum]} -> binary_to_integer(SNum);
				nomatch -> T
			end
		end, Params)}.
	
