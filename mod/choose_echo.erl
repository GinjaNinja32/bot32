-module(choose_echo).
-compile(export_all).

get_commands() ->
	[
		{"echodel", fun echodel/1, [short,long], admin}
	]
	++
	lists:map(fun(T) ->
			{atom_to_list(T), generic(T), user}
		end, config:get_value(config, [?MODULE, types], [])).

generic(T) ->
	fun
		(#{reply:=Reply, ping:=Ping, params:=["add"|S]}) when S /= []->
			String = list_to_binary(string:join(S, " ")),
			case lists:member(String, config:get_value(data, [?MODULE, types,T], [])) of
				true -> {irc, {msg, {Reply, [Ping, "That's already on the list."]}}};
				false ->
					config:mod_get_value(data, [?MODULE, types, T], fun
							('$none') -> [String];
							(X) -> [String|X]
						end),
					{irc, {msg, {Reply, [Ping, "Added."]}}}
			end;
		(#{reply:=Reply, ping:=Ping}) ->
			case config:get_value(data, [?MODULE, types, T], []) of
				[] -> {irc, {msg, {Reply, [Ping, io_lib:format("I don't have anything to give you for ~p.", [T])]}}};
				Options ->{irc, {msg, {Reply, [Ping, util:pick_rand(Options)]}}}
			end
	end.

echodel(#{reply:=Reply, ping:=Ping, params:=[Type,Str]}) ->
	T = list_to_atom(Type),
	String = list_to_binary(Str),
	case lists:member(String, config:get_value(data, [?MODULE, types,T], [])) of
		false -> {irc, {msg, {Reply, [Ping, "That's not on the list."]}}};
		true ->
			config:mod_get_value(data, [?MODULE, types, T], fun
					(X) -> lists:delete(String, X)
				end),
			{irc, {msg, {Reply, [Ping, "Removed."]}}}
	end.
