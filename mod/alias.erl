-module(alias).
-compile(export_all).
-compile({no_auto_import,[apply/2]}).

get_commands() ->
	[
		{"alias", fun alias/1, admin},
		{"isalias", fun isalias/1, user},
		{"unalias", fun unalias/1, admin}
	].

pre_command(Command, Args) ->
	LCommand = string:to_lower(Command),
	case config:get_value(data, [?MODULE, aliases, LCommand]) of
		'$none' -> {Command, Args};
		{V, _} when Args == none -> pre_command(V, none);
		{V, Spec} -> pre_command(V, apply(Spec, Args));
		V -> pre_command(V, Args)
	end.

apply(Spec, Args) ->
	lists:flatmap(fun
			({T}) when is_integer(T) -> lists:nthtail(T-1, Args);
			(T) when is_integer(T) -> [lists:nth(T, Args)];
			(T) -> [T]
		end, Spec).

alias(#{reply:=Reply, ping:=Ping, params:=Params}) ->
	case Params of
		[New, Real | ArgSpec] ->
			case parse(ArgSpec) of
				{ok,Spec} ->
					config:set_value(data, [?MODULE, aliases, New], {Real, Spec}),
					{irc, {msg, {Reply, [Ping, "Done."]}}};
				error ->
					{irc, {msg, {Reply, [Ping, "Illegal spec."]}}}
			end;
		_ -> {irc, {msg, {Reply, [Ping, "Usage: alias [alias] [command] [argspec]"]}}}
	end.

unalias(#{reply:=Reply, ping:=Ping, params:=Params}) ->
	case Params of
		[Alias] ->
			case config:get_value(data, [?MODULE, aliases, Alias]) of
				'$none' ->
					{irc, {msg, {Reply, [Ping, $', Alias, $', " is not an alias"]}}};
				_ ->
					config:del_value(data, [?MODULE, aliases, Alias]),
					{irc, {msg, {Reply, [Ping, "Done."]}}}
			end;
		_ -> {irc, {msg, {Reply, [Ping, "Usage: unalias [alias]"]}}}
	end.

isalias(#{reply:=Reply, ping:=Ping, params:=Params}) ->
	case Params of
		[Alias] ->
			case config:get_value(data, [?MODULE, aliases, Alias]) of
				'$none' ->
					{irc, {msg, {Reply, [Ping, io_lib:format("~s is not an alias.", [Alias])]}}};
				{Command,Spec} ->
					{irc, {msg, {Reply, [Ping, io_lib:format("~s is an alias for ~s with spec ~s.", [Alias, Command, format_spec(Spec)])]}}}
			end;
		_ -> {irc, {msg, {Reply, [Ping, "Usage: isalias [command]"]}}}
	end.

parse(Params) ->
	{ok, lists:map(fun(T) ->
		case re:run(T, <<"^(\\\\|\\*)([0-9]+)$">>, [{capture, all_but_first, binary}]) of
			{match, [<< "*">>, SNum]} -> {binary_to_integer(SNum)};
			{match, [<<"\\">>, SNum]} -> binary_to_integer(SNum);
			nomatch -> T
		end
	end, Params)}.

format_spec([]) -> "[no arguments]";
format_spec(Spec) ->
	string:join(lists:map(fun
			({T}) -> io_lib:format("[~b onwards]", [T]);
			(T) when is_number(T) -> io_lib:format("[~b]", [T]);
			(T) -> T
		end, Spec), " ").
