-module(burger).
-compile(export_all).

-include("definitions.hrl").

get_food_options() ->
	[
		{burger, [bun, filling, topping], ["puts ", bun, " on the table, piles on ", filling, ", ", filling, ", and ", filling, ", then tops it with ", topping, ", sliding it over to "]},
		{antag, [person, cloth, weapon, mission], ["dresses ", person, " into ", cloth, " and ", cloth, ", gives them ", weapon, " and ", weapon, ", and sends them on a mission to ", mission, " "]}
	].

get_tuple_for_key(K) ->
	lists:keyfind(K, 1, get_food_options()).

get_commands() ->
	lists:map(fun({K,_,_}) -> {atom_to_list(K), genfood(K), user} end, get_food_options())
	++
	[
		{"delfood", fun delfood/4, admin}
	].

get_help(String) ->
	LString = string:to_lower(String),
	case lists:filter(fun({K,_,_}) -> LString == atom_to_list(K) end, get_food_options()) of
		[{_, Options, Format}] ->
			[
				["Add with '", String, " add [key] [what]'."],
				["Key can be: ", string:join(lists:map(fun atom_to_list/1, Options), ", "), $.],
				["Format is '", lists:map(fun(T) when is_atom(T) -> [$[,atom_to_list(T),$]]; (T) -> T end, Format), "[Nick].'"]
			];
		_ -> unhandled
	end.

genfood(T) -> fun(O, RT, P, Prms) -> mkfood(T, O, RT, P, Prms) end.

mkfood(_, _, RT, P, ["add",_]) -> {irc, {msg, {RT, [P, "Provide an item!"]}}};
mkfood(K, _, RT, P, ["add",Key|What]) ->
	{_,Keys,_} = get_tuple_for_key(K),
	Food = config:get_value(data, [burger, K], []),
	case case lists:member(Key, lists:map(fun atom_to_list/1, Keys)) of
		true -> list_to_atom(Key);
		false -> "Invalid type!"
	end of
		RealKey when is_atom(RealKey) ->
			Entry = list_to_binary(string:join(What, " ")),
			case case orddict:find(RealKey, Food) of
				{ok, W} ->
					case lists:member(Entry, W) of
						true -> error;
						false -> [Entry | W]
					end;
				error -> [Entry]
			end of
				error -> {irc, {msg, {RT, [P, "That's already on the list."]}}};
				NewList ->
					NewFood = orddict:store(RealKey, NewList, Food),
					config:set_value(data, [burger, K], NewFood),
					{irc, {msg, {RT, [P, "Added."]}}}
			end;
		Error -> {irc, {msg, {RT, [P, Error]}}}
	end;

mkfood(K, O, RT, P, _) ->
	{_,_,String} = get_tuple_for_key(K),
	Message = case config:get_value(data, [burger, K], []) of
		[] -> [P, "Can't find my ",atom_to_list(K)," ingredients, sorry."];
		Food ->
			Msg = lists:map(fun(T) when is_atom(T) -> getrand(T, Food); (T) -> T end, String),
			["\x01ACTION ", Msg, O, ".\x01"]
	end,
	{irc, {msg, {RT, Message}}}.

delfood(_, RT, P, Params) when length(Params) < 3 -> {irc, {msg, {RT, [P, "Provide a type, key, and the exact string of what you want to remove."]}}};
delfood(_, RT, P, [T,K|W]) ->
	Data = config:get_value(data, [burger], []),
	Bin = list_to_binary(string:join(W, " ")),
	Msg = case orddict:find(list_to_atom(T), Data) of
		{ok, X} ->
			case orddict:find(list_to_atom(K), X) of
				{ok, Y} ->
					case lists:member(Bin, Y) of
						true ->
							NewList = lists:delete(Bin, Y),
							config:set_value(data, [burger, list_to_atom(T), list_to_atom(K)], NewList),
							"Removed.";
						false -> "Entry Not found."
					end;
				error -> "Key not found."
			end;
		error -> "Type not found."
	end,
	{irc, {msg, {RT, [P, Msg]}}}.

getrand(Key, Data) ->
	case orddict:find(Key, Data) of
		{ok, []} -> "##error";
		{ok, List} -> lists:nth(random:uniform(length(List)), List);
		error -> "##error"
	end.
