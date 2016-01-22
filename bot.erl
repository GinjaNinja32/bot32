-module(bot).
-compile(export_all).
-compile({no_auto_import,[load_module/2]}).

-include("definitions.hrl").

get_commands() ->
	[
		{"update", fun(#{reply:=Reply}) -> {update, Reply} end, host}
	].

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

check_utf8(<<>>) -> true;
check_utf8(<<_/utf8, B/binary>>) -> check_utf8(B);
check_utf8(_) -> false.

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
					case catch util:call_or(Module, handle_event, [msg, Params], null) of
						{'EXIT', X} -> logging:log(error, ?MODULE, "handle_event for ~p exited ~p", [Module, X]);
						{'EXIT', RS, X} -> logging:log(error, ?MODULE, "handle_event for ~p errored ~p (~p)", [Module, X, RS]);
						_ -> ok
					end
				end, config:get_value(config, [bot, modules])),
			case config:require_value(config, [bot, nick]) of
				Channel ->
					case permissions:hasperm(User, admin) of
						true -> ok;
						_ -> permissions:message_all_rank(["Query from ",Nick], string:join(Tokens, " "), pmlog)
					end;
				_ -> ok
			end
	end
	end;

handle_irc(nick, {U=#user{nick=OldNick}, NewNick}) ->
	case config:get_value(config, [bot, nick]) of
		OldNick -> config:set_value(config, [bot, nick], NewNick);
		_ ->
			lists:foreach(fun(Module) ->
					util:call_or(Module, handle_event, [nick, {U, NewNick}], null)
				end, config:require_value(config, [bot, modules]))
	end;

handle_irc(notice, _) -> ok;

handle_irc(numeric, {{rpl,away},_}) -> ok;
handle_irc(numeric, {{A,B},Params}) -> logging:log(info, ?MODULE, "Numeric received: ~p_~p ~s", [A,B,string:join(Params," ")]);

handle_irc(Type, Params) ->
	lists:foreach(fun(Module) ->
				util:call_or(Module, handle_event, [Type, Params], null)
			end, config:require_value(config, [bot, modules])).

