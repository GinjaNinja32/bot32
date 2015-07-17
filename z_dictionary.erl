-module(z_dictionary).
-compile(export_all).

get_commands() ->
	[
		{"dictionary", fun dict/5, user},
		{"definition", fun dict/5, user},
		{"define", fun dict/5, user}
	].

initialise(T) -> T.
deinitialise(T) -> T.


dict(_, RT, P, Params, _) -> {irc, {msg, {RT, [P, dictionary(Params)]}}}.

dictionary([]) -> "Provide a word to look up.";
dictionary(Words) ->
	os:putenv("word", string:join(Words, " ")),
	Reply = os:cmd("dict -fd wn $word"),
	parse_reply(Reply).


parse_reply(Text) ->
	Lines = string:tokens(Text, "\n"),
	case lists:prefix("No definitions found for", hd(Lines)) of
		true ->
			case lists:map(fun(T) -> re:replace(T, "^([a-zA-Z0-9\.]+\t){3}", "", [{return, list}]) end, tl(Lines)) of
				[] -> "No definition found.";
				Cleaned -> ["No definition found, did you mean: ", string:join(Cleaned, " ")]
			end;
		false ->
			Definitions = lists:map(fun(T) -> re:replace(T, "^[ \t]*", "", [{return, list}]) end, tl(tl(Lines))),
			FirstDef = lists:takewhile(fun(T) -> not lists:prefix("2: ", T) end, Definitions),
			re:replace(string:join(FirstDef, " "), "^([A-Za-z]+) ([A-Za-z]+) 1:", "\\1 (\\2)", [{return, binary}])
	end.
