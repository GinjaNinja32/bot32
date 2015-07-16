-module(z_notes).
-compile(export_all).

-include("definitions.hrl").

get_commands() ->
	[
		{"delnote", fun delnote/5, user},
		{"remnote", fun delnote/5, user},
		{"note", fun note/5, user}
	].

get_data(#state{moduledata=M}) ->
	case orddict:find(?MODULE, M) of
		{ok, Value} -> Value;
		error -> orddict:new()
	end.

set_data(S=#state{moduledata=M}, Data) ->
	S#state{moduledata=orddict:store(?MODULE, Data, M)}.

store_data(Data) ->
	bot ! {setkey, {?MODULE, Data}},
	ok.

initialise(T) ->
	Data = load_data(),
	set_data(T, Data).

deinitialise(T) ->
	save_data(get_data(T)),
	T#state{moduledata=orddict:erase(?MODULE, T#state.moduledata)}.

%

delnote(O, RT, P, [T], S) ->
	Dat = get_data(S),
	LO = string:to_lower(O),
	{Reply, UDict} = case orddict:find(LO, Dat) of
		{ok, Value} ->
			case orddict:find(T, Value) of
				{ok, _} -> {"Note deleted.", orddict:erase(T, Value)};
				error -> {"Note not found.", Value}
			end;
		error -> {"You have no notes.", orddict:new()}
	end,
	NewDict = case UDict of
		[] -> orddict:erase(LO, Dat);
		_ -> orddict:store(LO, UDict, Dat)
	end,
	core ! {irc, {msg, {RT, [P, Reply]}}},
	{state, set_data(S, NewDict)};
delnote(_, RT, P, _, _) -> {irc, {msg, {RT, [P, "Provide a single note key!"]}}}.

note(O, RT, P, ["list"], S) -> notes(O, RT, P, a, S);
note(O, RT, P, [T], S) ->
	Dat = get_data(S),
	LO = string:to_lower(O),
	Reply = case orddict:find(LO, Dat) of
		{ok, Value} ->
			case orddict:find(T, Value) of
				{ok, Note} -> Note;
				error -> ["You do not have a note '", T, "'."]
			end;
		error -> "You do not have any notes."
	end,
	{irc, {msg, {RT, [P, Reply]}}};
note(O, RT, P, [T|C], S) ->
	Dat = get_data(S),
	LO = string:to_lower(O),
	{Reply, UDict} = case orddict:find(LO, Dat) of
		{ok, Value} ->
			Len = orddict:size(Value),
			if
				Len < 20 ->
					case orddict:find(T, Value) of
						{ok, _} -> {"Note is already set!", Value};
						error -> {"Note added.", orddict:store(T, string:join(C, " "), Value)}
					end;
				true ->
					{"You have too many notes; delete some before adding more!", Value}
			end;
		error -> {"Note added.", orddict:store(T, string:join(C, " "), orddict:new())}
	end,
	core ! {irc, {msg, {RT, [P, Reply]}}},
	{state, set_data(S, orddict:store(LO, UDict, Dat))};
note(_, RT, P, _, _) -> {irc, {msg, {RT, [P, "Provide either a key (to retrieve) or a key and a string (to set)!"]}}}.

notes(O, RT, P, _, S) ->
	Dat = get_data(S),
	LO = string:to_lower(O),
	Reply = case case orddict:find(LO, Dat) of
		{ok, Value} -> orddict:fetch_keys(Value);
		error -> []
	end of
		[] -> "You have no notes.";
		T -> string:join(lists:map(fun(X)->[$',X,$'] end, T), " ")
	end,
	{irc, {msg, {RT, [P, Reply]}}}.

load_data() ->
	case file:consult("notes.crl") of
		{ok, [Notes]} -> Notes;
		_ -> orddict:new()
	end.

save_data(Dat) ->
	T = file:write_file("notes.crl", io_lib:format("~p.~n", [Dat])),
	logging:log(info, "NOTES", "Save status: ~p", [T]).
