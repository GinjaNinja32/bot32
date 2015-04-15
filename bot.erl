-module(bot).
-compile(export_all).

-include("definitions.hrl").

-record(config, {nick, prefix, admins, ignore, user, mode, real, channels, on_join}).

-define(MODULES, [z_basic, z_dice, z_message, z_status, z_admin]).

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
	BaseConfig = #config{nick="Bot32", prefix=$!, admins=sets:new(), user="Bot32", mode="0", real="Bot32", channels=sets:new(), ignore=sets:new(), on_join=[]},
	case file:consult("bot_config.erl") of
		{error, Reason} ->
			common:debug("BOT", "Failed to load config file: ~p.", [Reason]),
			UseConfig = BaseConfig;
		{ok, Terms} ->
			UseConfig = lists:foldl(fun(Option, Config=#config{admins=A, on_join=OJ, channels=C, ignore=I}) ->
					case Option of
						{nick, Nick}		when is_list(Nick) orelse is_binary(Nick)	-> Config#config{nick=Nick};
						{user, User}		when is_list(User) orelse is_binary(User)	-> Config#config{user=User};
						{mode, Mode}		when is_list(Mode) orelse is_binary(Mode)	-> Config#config{mode=Mode};
						{real, Real}		when is_list(Real) orelse is_binary(Real)	-> Config#config{real=Real};

						{prefix, Prefix}	when is_integer(Prefix)				-> Config#config{prefix=Prefix};

						{admin, Admin}		when is_list(Admin) orelse is_binary(Admin)	-> Config#config{admins=sets:add_element(string:to_lower(Admin), A)};
						{admins, Admins}	when is_list(Admins)				-> Config#config{admins=lists:foldl(fun sets:add_element/2, A, lists:map(fun string:to_lower/1, Admins))};

						{channel, Channel}	when is_list(Channel) orelse is_binary(Channel)	-> Config#config{channels=sets:add_element(Channel, C)};
						{channels, Channels}	when is_list(Channels)				-> Config#config{channels=lists:foldl(fun sets:add_element/2, C, Channels)};

						{ignore, Ignore}	when is_list(Ignore) orelse is_binary(Ignore)	-> Config#config{ignore=sets:add_element(string:to_lower(Ignore), I)};
						{ignores, Ignores}	when is_list(Ignores)				-> Config#config{ignore=lists:foldl(fun sets:add_element/2, I, lists:map(fun string:to_lower/1, Ignores))};

						{on_join, Cmd}		when is_tuple(Cmd)				-> Config#config{on_join = [Cmd | OJ]};

						T -> common:debug("BOT", "Failed to parse config line ~p!", [T])
					end
				end, BaseConfig, Terms)
	end,
	Commands = load_commands(),
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
	State = lists:foldl(fun(Module, S) ->
			apply(Module, initialise, [S])
		end,  #state{
				nick     = UseConfig#config.nick,
				prefix   = UseConfig#config.prefix,
				admins   = UseConfig#config.admins,
				ignore   = UseConfig#config.ignore,
				commands = Commands,
				moduledata  = orddict:new()
			    }, ?MODULES),
	case loop(State) of
		FinalState=#state{} ->
			lists:foreach(fun(Module, S) ->
					apply(Module, deinitialise, [S])
				end, FinalState, ?MODULES);
		T -> common:debug("BOT", "quitting under condition ~p", [T])
	end,
	common:debug("BOT", "quitting").

reinit(State) ->
	register(bot, self()),
	common:debug("BOT", "starting"),
	loop(State),
	common:debug("BOT", "quitting").

loop(State = #state{}) ->
	case receive
		{irc, {Type, Params}} -> handle_irc(Type, Params, State);
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
		update -> spawn(common,purge_call,[bot,reinit, State]), ok;
		S -> common:debug("BOT", "unknown code ~p, continuing", [S]), loop(State)
	end.

message_admins(Category, Msg, Admins) ->
	common:debug("ADMIN", [Category, ": ", Msg]),
	lists:foreach(fun(T) ->
			core ! {irc, {msg, {T, [Category, ": ", Msg]}}}
		end, sets:to_list(Admins)),
	ok.

is_ignored(#user{nick=N}, Ignored) ->
	sets:is_element(string:to_lower(N), Ignored).

is_admin(#user{nick=N}, Admins) ->
	sets:is_element(string:to_lower(N), Admins).

handle_irc(msg, {User=#user{nick=Nick}, Channel, Tokens}, State=#state{nick=MyNick, prefix=Prefix, admins=Admins, ignore=Ignored}) ->
	case is_ignored(User, Ignored) of
		true -> ok;
		false ->
			case Channel of
				MyNick ->
					ReplyChannel = Nick,
					ReplyPing = "",
					case is_admin(User, Admins) of
						false -> message_admins(["Query from ",Nick], string:join(Tokens, " "), Admins);
						true -> ok
					end;
				_ ->
					ReplyChannel = Channel,
					ReplyPing = Nick ++ ": "
			end,
			case hd(Tokens) of
				[Prefix | Command] ->
					case is_admin(User, Admins) of
						true -> handle_admin_command(Nick, ReplyChannel, ReplyPing, Command, tl(Tokens), State);
						false -> handle_command(Nick, ReplyChannel, ReplyPing, Command, tl(Tokens), State)
					end;
				X ->
					case lists:prefix(string:to_lower(MyNick), string:to_lower(X)) of
						true ->	case is_admin(User, Admins) of
								true -> handle_admin_command(Nick, ReplyChannel, ReplyPing, hd(tl(Tokens)), tl(tl(Tokens)), State);
								false -> handle_command(Nick, ReplyChannel, ReplyPing, hd(tl(Tokens)), tl(tl(Tokens)), State)
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
handle_irc(nick, {_, N}, S=#state{}) ->
	z_message:check_messages_for(N, S),
	ok;

handle_irc(join, {#user{nick=N}, _Channel}, S=#state{}) ->
	z_message:check_messages_for(N, S),
	ok;

handle_irc(topic, _, _) -> ok;
handle_irc(kick, _, _) -> ok;
handle_irc(notice, _, _) -> ok;
handle_irc(part, _, _) -> ok;
handle_irc(quit, _, _) -> ok;
handle_irc(mode, _, _) -> ok;

handle_irc(Type, Params, _State) -> common:debug("BOT", "unknown irctype ~p <<<~p>>>, continuing", [Type, Params]).

handle_admin_command(Origin, ReplyTo, Ping, Cmd, Params, State=#state{commands={AdminCmd,_}}) ->
	case string:to_lower(Cmd) of
		"update" ->		update;
		"help" ->		core ! {irc, {msg, {ReplyTo, ["Admin commands: update, reloadcommands, help, ", string:join(orddict:fetch_keys(AdminCmd), ", "), "."]}}},
						handle_command(Origin, ReplyTo, Ping, Cmd, Params, State);

		"reloadcommands" ->
			NewCmds = load_commands(),
			self() ! {state, State#state{commands=NewCmds}},
			{irc, {msg, {ReplyTo, [Ping, "Reloaded commands."]}}};

		T -> case orddict:find(T, AdminCmd) of
			{ok, Result} -> apply(Result, [Origin, ReplyTo, Ping, Params, State]);
			error -> handle_command(Origin, ReplyTo, Ping, Cmd, Params, State)
		end
	end.

handle_command(Origin, ReplyTo, Ping, Cmd, Params, State=#state{commands={_,UserCmd}}) ->
	case string:to_lower(Cmd) of
		"help" -> {irc, {msg, {ReplyTo, ["User commands: help, ", string:join(orddict:fetch_keys(UserCmd), ", "), "."]}}};

		T -> case orddict:find(T, UserCmd) of
			{ok, Result} -> apply(Result, [Origin, ReplyTo, Ping, Params, State]);
			error -> {irc, {msg, {ReplyTo, [Ping, "Unknown command!"]}}}
		end
	end.

load_commands() ->
	lists:foldl(fun(T,AT) ->
			lists:foldl(fun({Cmd,Fun,Restrict},{Admin,User}) ->
				case Restrict of
					user -> {Admin,orddict:store(Cmd,Fun,User)};
					admin -> {orddict:store(Cmd,Fun,Admin),User};
					_ ->
						common:debug("CMD", "Unknown command restriction ~p", [Restrict]),
						{Admin,User}
				end
			end, AT, apply(T, get_commands, []))
		end, {orddict:new(), orddict:new()}, ?MODULES).
