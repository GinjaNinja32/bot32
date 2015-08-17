-module(z_dictionary).
-compile(export_all).

get_aliases() ->
	[
		{"dictionary", ["definition", "define"]}
	].

get_commands() ->
	[
		{"dictionary", fun dict/5, user}
	].

initialise(T) -> T.
deinitialise(T) -> T.


dict(_, RT, P, Params, _) -> {irc, {msg, {RT, [P, dictionary(Params)]}}}.

dictionary([]) -> "Provide a word to look up.";
dictionary(Words) ->
	case re:run(hd(Words), "[0-9]+", [{capture, none}]) of
		match -> N = list_to_integer(hd(Words)), Word = tl(Words);
		nomatch -> N = 1, Word = Words
	end,
	case Word of
		[] -> "Provide a word to look up.";
		_ ->
			os:putenv("word", string:join(Word, " ")),
			Reply = os:cmd("dict -fd wn \"$word\""),
			parse_reply(Reply, N)
	end.

parse_reply(Text, N) ->
	Lines = string:tokens(Text, "\n"),
	case lists:prefix("No definitions found for", hd(Lines)) of
		true ->
			case lists:map(fun(T) -> re:replace(T, "^([a-zA-Z0-9\.]+\t){3}", "", [{return, list}]) end, tl(Lines)) of
				[] -> "No definition found.";
				Cleaned -> ["No definition found, did you mean: ", string:join(Cleaned, " ")]
			end;
		false ->
			{Term, Meanings} = parse_dictionary(tl(tl(Lines))),
			case Meanings of
				error -> logging:log(error, "DICTIONARY", "Failed to parse:~n~p", [tl(tl(Lines))]), "Failed to parse reply.";
				_ ->
					if
						N =< length(Meanings) ->
							case length(Meanings) of
								1 -> io_lib:format("~s ~s", [Term, lists:nth(N, Meanings)]);
								Num -> io_lib:format("(~b of ~b): ~s ~s", [N, Num, Term, lists:nth(N, Meanings)])
							end;
						true ->
							io_lib:format("There are only ~b definitions!", [length(Meanings)])
					end
			end
	end.

parse_dictionary(DefinitionLines) ->
	DefinedTerm = string:strip(hd(DefinitionLines)),
	Definitions = parse_defines(tl(DefinitionLines), "n", []),
	{DefinedTerm, Definitions}.

parse_defines([], _, Defs) -> lists:reverse(Defs);
parse_defines(Lines, CType, Defs) ->
	case case re:run(hd(Lines), "      (?:([a-z]+) )?[0-9]+: (.*)", [{capture, all_but_first, list}]) of
		nomatch -> Type=x, Str=x, error;
		{match, [[], Str]} -> Type = CType, ok;
		{match, [Type, Str]} -> ok
	end of
		error -> error;
		ok ->
			{Def, Rest} = lists:splitwith(fun(T) -> lists:prefix("         ", T) end, tl(Lines)),
			parse_defines(Rest, Type, [[$(, Type, $), $ , string:strip(Str), $ , string:join(lists:map(fun string:strip/1, Def), " ")] | Defs])
	end.
