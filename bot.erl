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

	config:set_value(temp, [bot, commands], []),

	Modules = config:get_value(config, [bot, modules]),
	config:set_value(config, [bot, modules], []),
	logging:log(info, ?MODULE, "module load stat: ~s", [modules:load_modules(Modules)]),
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
		{request_execute, {Pid, Fun}} ->
			Pid ! {execute_done, catch Fun()},
			bot:loop();
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

desel(A,B) ->
	case lists:splitwith(fun(T)->T /= $@ end, A) of
		{Cmd, [_|Sel]} -> {Cmd, B, Sel};
		{Cmd, []} -> {Cmd, B, []}
	end.

parse_command([],_) -> notcommand;
parse_command(Params, IsQuery) ->
	<<FirstChar/utf8, Rest/binary>> = list_to_binary(hd(Params)),
	case lists:member(FirstChar, config:require_value(config, [bot, prefix])) of
		true when Rest /= <<>> -> desel(binary_to_list(Rest), tl(Params));
		true -> notcommand;
		false ->
			if
				length(Params) > 1 ->
					BotAliases = [config:require_value(config, [bot, nick]) | config:require_value(config, [bot, names])],
					case lists:any(fun(Alias) -> R=util:regex_escape(Alias), re:run(hd(Params), <<"^", R/binary, "($|[^a-zA-Z0-9])">>, [caseless, {capture, none}]) == match end, BotAliases) of
						true -> case tl(Params) of
								[] -> {[], [], []};
								_ -> desel(hd(tl(Params)), tl(tl(Params)))
							end;
						false ->
							notcommand
					end;
			%	IsQuery -> {hd(Params),tl(Params)};
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
				{RCommand, RArguments, Selector} ->
					logging:log(info, ?MODULE, "Command in ~s from ~s: ~s ~s", [Channel, User#user.nick, RCommand, string:join(RArguments, " ")]),
					{Command, Arguments} = lists:foldl(fun(Module, {C,A}) ->
							call_or(Module, pre_command, [C,A], {C,A})
						end, {RCommand, RArguments}, config:require_value(config, [bot, modules])),
					Rank = permissions:rankof(User, ReplyChannel),
					case permissions:hasperm(User, host) of
						true -> handle_host_command(Rank, User, ReplyChannel, ReplyPing, Command, Arguments, Selector);
						false ->     handle_command(Rank, User, ReplyChannel, ReplyPing, Command, Arguments, Selector)
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

handle_host_command(Rank, User, ReplyTo, Ping, Cmd, Params, Selector) ->
	case string:to_lower(Cmd) of
		"update" ->		{update, ReplyTo};
		"help" ->	if Params == [] ->
						core ! {irc, {msg, {User#user.nick, ["builtin host commands: update, reload_all, drop_all, load_mod, drop_mod, reload_mod"]}}};
						true -> ok
					end,
					handle_command(Rank, User, ReplyTo, Ping, Cmd, Params, Selector);

		_ -> handle_command(Rank, User, ReplyTo, Ping, Cmd, Params, Selector)
	end.

handle_command(Ranks, User, ReplyTo, Ping, Cmd, Params, Selector) ->
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
			{HelpCommand, _} = lists:foldl(fun(Module, {C,A}) ->
					call_or(Module, pre_command, [C,A], {C,A})
				end, {hd(Params), none}, config:require_value(config, [bot, modules])),
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
											params => Params,
											selector => Selector
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

call_or(Mod, Func, Args, Or) ->
	util:call_or(Mod, Func, Args, Or).

