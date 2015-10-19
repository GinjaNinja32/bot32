-module(notes).
-compile(export_all).

-include("definitions.hrl").

get_commands() ->
	[
		{"delnote", fun delnote/4, user},
		{"remnote", fun delnote/4, user},
		{"note", fun note/4, user}
	].

%

delnote(O, RT, P, [T]) ->
	LO = string:to_lower(O),
	{Reply, UDict} = case config:get_value(data, [?MODULE, LO]) of
		'$none' -> {"You have no notes.", []};
		Value ->
			case orddict:find(T, Value) of
				{ok, _} -> {"Note deleted.", orddict:erase(T, Value)};
				error -> {"Note not found.", Value}
			end
	end,
	config:set_value(data, [?MODULE, LO], UDict),
	{irc, {msg, {RT, [P, Reply]}}};
delnote(_, RT, P, _) -> {irc, {msg, {RT, [P, "Provide a single note key!"]}}}.

note(O, RT, P, ["list"]) -> notes(O, RT, P, a);
note(O, RT, P, [T]) ->
	LO = string:to_lower(O),
	Reply = case config:get_value(data, [?MODULE, LO]) of
		'$none' -> "You do not have any notes.";
		Value ->
			case orddict:find(T, Value) of
				{ok, Note} -> Note;
				error -> ["You do not have a note '", T, "'."]
			end
	end,
	{irc, {msg, {RT, [P, Reply]}}};
note(O, RT, P, [T|C]) ->
	LO = string:to_lower(O),
	{Reply, UDict} = case config:get_value(data, [?MODULE, LO]) of
		'$none' -> {"Note added.", orddict:store(T, string:join(C, " "), orddict:new())};
		Value ->
			Len = orddict:size(Value),
			if
				Len < 20 ->
					case orddict:find(T, Value) of
						{ok, _} -> {"Note is already set!", Value};
						error -> {"Note added.", orddict:store(T, string:join(C, " "), Value)}
					end;
				true ->
					{"You have too many notes; delete some before adding more!", Value}
			end
	end,
	config:set_value(data, [?MODULE, LO], UDict),
	{irc, {msg, {RT, [P, Reply]}}};
note(_, RT, P, _) -> {irc, {msg, {RT, [P, "Provide either a key (to retrieve) or a key and a string (to set)!"]}}}.

notes(O, RT, P, _) ->
	LO = string:to_lower(O),
	Reply = case orddict:fetch_keys(config:get_value(data, [?MODULE, LO], [])) of
		[] -> "You have no notes.";
		T -> string:join(lists:map(fun(X)->[$',X,$'] end, T), " ")
	end,
	{irc, {msg, {RT, [P, Reply]}}}.
