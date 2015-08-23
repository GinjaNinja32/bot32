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
		{"delfood", fun delfood/5, admin},
		{"load_food", fun load_food/5, host},
		{"save_food", fun save_food/5, host}
	].

default_data() -> orddict:new().
data_persistence() -> automatic.
-include("basic_module.hrl").

genfood(T) -> fun(O, RT, P, Prms, S) -> mkfood(T, O, RT, P, Prms, S) end.

mkfood(K, _, RT, P, ["help"|_], _) ->
	{_, Keys, String} = get_tuple_for_key(K),
	{irc, {msg, {RT, [P,
		"Add with '",atom_to_list(K)," add [key] [what]'. ",
		"Key can be: ", string:join(lists:map(fun atom_to_list/1, Keys), ", "), ". ",
		"Format is '", lists:map(fun(T) when is_atom(T) -> [$[,atom_to_list(T),$]]; (T) -> T end, String), "[Nick].'"
	]}}};

mkfood(_, _, RT, P, ["add",_], _) -> {irc, {msg, {RT, [P, "Provide an item!"]}}};
mkfood(K, _, RT, P, ["add",Key|What], S) ->
	{_,Keys,_} = get_tuple_for_key(K),
	Data = get_data(S),
	Food = case orddict:find(K,  Data) of
		{ok, V} -> V;
		error -> orddict:new()
	end,
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
					NewData = orddict:store(K, NewFood, Data),
					save_data(NewData),
				%	store_data(NewData),
					core ! {irc, {msg, {RT, [P, "Added."]}}},
					{setkey, {?MODULE, NewData}}
			end;
		Error -> {irc, {msg, {RT, [P, Error]}}}
	end;

mkfood(K, O, RT, P, _, S) ->
	{_,_,String} = get_tuple_for_key(K),
	Data = get_data(S),
	Message = case orddict:find(K, Data) of
		{ok, Food} ->
			Msg = lists:map(fun(T) when is_atom(T) -> getrand(T, Food); (T) -> T end, String),
			["\x01ACTION ", Msg, O, ".\x01"];
		error -> [P, "Can't find my ",atom_to_list(K)," ingredients, sorry."]
	end,
	{irc, {msg, {RT, Message}}}.

delfood(_, RT, P, Params, _) when length(Params) < 3 -> {irc, {msg, {RT, [P, "Provide a type, key, and the exact string of what you want to remove."]}}};
delfood(_, RT, P, [T,K|W], S) ->
	Data = get_data(S),
	Bin = list_to_binary(string:join(W, " ")),
	Msg = case orddict:find(list_to_atom(T), Data) of
		{ok, X} ->
			case orddict:find(list_to_atom(K), X) of
				{ok, Y} ->
					case lists:member(Bin, Y) of
						true ->
							NewList = lists:delete(Bin, Y),
							NewX = orddict:store(list_to_atom(K), NewList, X),
							NewData = orddict:store(list_to_atom(T), NewX, Data),
							save_data(NewData),
							store_data(NewData),
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

load_food(_, RT, P, _, _) ->
	store_data(bot:modload_auto(?MODULE)),
	{irc, {msg, {RT, [P, "Loaded food data."]}}}.

save_food(_, RT, P, _, S) ->
	save_data(get_data(S)),
	{irc, {msg, {RT, [P, "Saved food data."]}}}.

save_data(D) ->
	T = file:write_file("modules/burger.crl", io_lib:format("~p.~n", [D])),
	logging:log(info, "BURGER", "Save status: ~p", [T]).
