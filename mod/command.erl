-module(command).
-compile(export_all).

-include("definitions.hrl").

handle_event(msg, Params = {User=#user{nick=Nick}, Channel, Tokens}) ->
	case parse_command(Tokens) of
		{RCommand, RArguments, Selector} ->
			logging:log(info, ?MODULE, "Command in ~s from ~s: ~s~s ~s", [Channel, Nick, RCommand, if Selector /= [] -> [$@|Selector]; true -> [] end, string:join(RArguments, " ")]),
			{Command, Arguments} = lists:foldl(fun(Module, {C,A}) ->
					util:call_or(Module, pre_command, [C, A], {C, A})
				end, {RCommand, RArguments}, config:require_value(config, [bot, modules])),
			Rank = permissions:rankof(User, Channel),
			case handle_command(Rank, User, Channel, Command, Arguments, Selector) of
				{irc, Result} ->
					core ! {irc, Result};
				{'EXIT', Term} ->
					logging:log(error, ?MODULE, "handle_command exited ~p", [Term]);
				_ -> ok
			end;
		notcommand ->
			RC = reply_channel(Nick, Channel),
			RP = reply_ping(Nick, RC),
			lists:foreach(fun(Module) ->
					util:call_or(Module, do_extras, [Tokens, RC, RP], null),
					util:call_or(Module, handle_event, [msg_nocommand, Params], null)
				end, config:require_value(config, [bot, modules]))
	end;
handle_event(_, _) -> ok.

parse_command([]) -> notcommand;
parse_command(Params) ->
	<<FirstChar/utf8, Rest/binary>> = list_to_binary(hd(Params)),
	case lists:member(FirstChar, config:require_value(config, [bot, prefix])) of
		true when Rest /= <<>> -> desel(binary_to_list(Rest), tl(Params));
		true -> notcommand;
		false ->
			if
				length(Params) > 1 ->
					BotAliases = [config:require_value(config, [bot, nick]) | config:require_value(config, [bot, names])],
					case lists:any(fun(Alias) ->
								R = util:regex_escape(Alias),
								re:run(hd(Params), <<"^", R/binary, "($|[^a-zA-Z0-9])">>, [caseless, {capture, none}]) == match
							end, BotAliases) of
						true ->
							case tl(Params) of
								[] -> {[], [], []};
								_ -> desel(hd(tl(Params)), tl(tl(Params)))
							end;
						false -> notcommand
					end;
				true -> notcommand
			end
	end.

desel(A, B) ->
	case lists:splitwith(fun(T) -> T /= $@ end, A) of
		{Cmd, [_|Sel]} -> {Cmd, B, Sel};
		{Cmd, []} -> {Cmd, B, []}
	end.

reply_channel(Nick, Channel) ->
	case config:require_value(config, [bot, nick]) of
		Channel -> Nick;
		_ -> Channel
	end.

reply_ping(Nick, Channel) ->
	if
		Channel == Nick -> "";
		true ->
			case config:get_value(data, [call, string:to_lower(Nick)]) of
				'$none' -> [Nick, ": "];
				Nickname -> ["\x0F", Nickname, "\x0F: "]
			end
	end.

describe_spec(Command, Spec) ->
	["Usage: ", Command, " ",
		string:join(lists:map(fun
				({_,ignore}) -> "(ignored)";
				(ignore) -> "(ignored)";
				({Name,_}) -> [$<,Name,$>];
				(Type) -> [$[,atom_to_list(Type),$]]
			end, Spec), " ")].

purespec(Spec) ->
	lists:map(fun
			({_Name,Type}) -> Type;
			(Type) -> Type
		end, Spec).

get_args(Spec, Args) ->
	case get_args(purespec(Spec), Args, []) of
		T when is_list(T) -> lists:reverse(T);
		T -> T
	end.


get_args([], [], X) -> X;
get_args([], _, _) -> {error, "Too many arguments provided."};
get_args(_, [], _) -> {error, "Not enough arguments provided."};

get_args([short|SRst],   [T|ARst], X) -> get_args(SRst,ARst,[T|X]);

get_args([ignore], _, X) -> X;
get_args([ignore|SRst], [_|ARst], X) -> get_args(SRst, ARst, X);

get_args([long], [], _) -> {error, "Not enough arguments provided."};
get_args([long], Rst, X) -> [string:join(Rst, " ") | X];
get_args([long|_], _, _) -> {error, "Invalid argument specification (bot bug)"};

get_args([list], [], _) -> {error, "Not enough arguments provided."};
get_args([list], Rst, X) -> [Rst | X];
get_args([list|_], _, _) -> {error, "Invalid argument specification (bot bug)"};

get_args([integer|SRst], [T|ARst], X) ->
	try
		Z = list_to_integer(T),
		get_args(SRst, ARst, [Z|X])
	catch
		error:badarg -> {error, io_lib:format("Invalid integer ~s", [T])}
	end;
get_args([Unk|_], _, _) -> {error, io_lib:format("Unknown or invalid argument type ~p (bot bug)", [Unk])}.

handle_command(Ranks, User, Channel, Command, Arguments, Selector) ->
	RC = reply_channel(User#user.nick, Channel),
	RP = reply_ping(User#user.nick, RC),
	Result = lists:foldl(fun
			(Rank, unhandled) ->
				case case config:get_value(temp, [bot, commands, Rank, string:to_lower(Command)]) of
					{_Mod, Func, ArgSpec} ->
						case get_args(ArgSpec, Arguments) of
							{error, T} ->
								core ! {irc, {msg, {RC, [RP, "Error: ",T]}}},
								error;
							X ->
								{Func, X}
						end;
					{_Mod, Func} -> {Func, Arguments};
					_ -> ok
				end of
					{Fn, Args} ->
						ParamMap = #{
								origin => User,
								nick => User#user.nick,
								reply => RC,
								ping => RP,
								params => Args,
								selector => Selector,
								ranks => Ranks
							},
						Fn(ParamMap);
					_ -> unhandled
				end;
			(_, Result) -> Result
		end, unhandled, Ranks),
	case Result of
		unhandled ->
			case alternate_commands([Command | Arguments]) of
				false -> ok;
				R ->
					RC = reply_channel(User#user.nick, Channel),
					RP = reply_ping(User#user.nick, RC),
					{irc, {msg, {RC, [RP, R]}}}
			end;
		_ -> Result
	end.

alternate_commands(Tokens) ->
	AltFunctions = lists:foldl(fun
			(Mod, Alt) ->
				Alt ++ util:call_or(Mod, alt_funcs, [], [])
		end, [], config:require_value(config, [bot, modules])),
	lists:foldl(fun
			(Func, false) ->
				case Func(Tokens) of
					false -> false;
					T ->
%						io:fwrite("~p(~p) returned ~p, HANDLED\n", [Func, Tokens, T]),
						T
				end;
			(_, Re) -> Re
		end, false, AltFunctions).

get_commands() ->
	[
		{"help", fun help/1, user}
	].

help(#{origin:=User, nick:=Nick, reply:=Reply, ping:=Ping, params:=Params}) ->
	case Params of
		[] ->
			lists:foreach(fun(Rank) ->
					help_list_commands(Nick, Rank, orddict:fetch_keys(config:get_value(temp, [bot, commands, Rank], [])))
				end, permissions:rankof(User, Reply));
		_ ->
			{HelpCommand, _} = lists:foldl(fun
					(Module, {C, A}) -> util:call_or(Module, pre_command, [C,A], {C,A})
				end, {hd(Params), none}, config:require_value(config, [bot, modules])),
			HelpTopic = string:join([HelpCommand | tl(Params)], " "),
			case lists:foldl(fun
						(Rank, unhandled) ->
							case config:get_value(temp, [bot, commands, Rank, HelpCommand]) of
								{Mod, _, Spec} ->
									Strings = [describe_spec(HelpCommand, Spec) | util:call_or(Mod, get_help, [HelpTopic], [])],
									core ! {irc, {msg, {Nick, ["Help for '", HelpTopic, "':"]}}},
									lists:foreach(fun(X) -> core ! {irc, {msg, {Nick, X}}} end, Strings);
								{Mod, _} ->
									case util:call_or(Mod, get_help, [HelpTopic], unhandled) of
										unhandled -> unhandled;
										Strings ->
											core ! {irc, {msg, {Nick, ["Help for '", HelpTopic, "':"]}}},
											lists:foreach(fun(X) -> core ! {irc, {msg, {Nick, X}}} end, Strings),
											ok
									end;
								_ -> unhandled
							end;
						(_, Result) -> Result
					end, unhandled, permissions:rankof(User, Reply)) of
				unhandled -> core ! {irc, {msg, {Reply, [Ping, "No help found for '", HelpTopic, "'."]}}};
				_ -> ok
			end
	end,
	ok.

help_list_commands(_, _, []) -> ok;
help_list_commands(Nick, Rank, Commands) when length(Commands) < 35 ->
	core ! {irc, {msg, {Nick, [io_lib:format("~p commands: ",[Rank]), string:join(Commands, ", "), "."]}}};
help_list_commands(Nick, Rank, Commands) ->
	{A, B} = lists:split(35, Commands),
	core ! {irc, {msg, {Nick, [io_lib:format("~p commands: ",[Rank]), string:join(A, ", "), "."]}}},
	help_list_commands(Nick, Rank, B).
