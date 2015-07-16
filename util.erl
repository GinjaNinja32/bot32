-module(util).
-compile(export_all).

regex_escape(String) ->
	lists:foldl(fun([F,R], Str) -> re:replace(Str, F, R, [{return, binary}, global]) end, String,
		[
			[<<"\\\\">>, <<"\\\\\\\\">>],
			[<<"\\^">>, <<"\\\\^">>],
			[<<"\\$">>, <<"\\\\$">>],
			[<<"\\.">>, <<"\\\\.">>],
			[<<"\\[">>, <<"\\\\[">>],
			[<<"\\]">>, <<"\\\\]">>],
			[<<"\\(">>, <<"\\\\(">>],
			[<<"\\)">>, <<"\\\\)">>],
			[<<"\\?">>, <<"\\\\?">>],
			[<<"\\*">>, <<"\\\\*">>],
			[<<"\\+">>, <<"\\\\+">>],
			[<<"\\{">>, <<"\\\\{">>],
			[<<"\\-">>, <<"\\\\-">>]
		]).

regex_star(String) ->
	lists:foldl(fun([F,R], Str) -> re:replace(Str, F, R, [global, {return, binary}, global]) end, String,
		[
			[<<"\\\\">>, <<"\\\\\\\\">>],
			[<<"\\^">>, <<"\\\\^">>],
			[<<"\\$">>, <<"\\\\$">>],
			[<<"\\.">>, <<"\\\\.">>],
			[<<"\\[">>, <<"\\\\[">>],
			[<<"\\]">>, <<"\\\\]">>],
			[<<"\\(">>, <<"\\\\(">>],
			[<<"\\)">>, <<"\\\\)">>],
			[<<"\\?">>, <<"\\\\?">>],
			[<<"\\+">>, <<"\\\\+">>],
			[<<"\\{">>, <<"\\\\{">>],
			[<<"\\-">>, <<"\\\\-">>],
			[<<"\\*">>, <<".*">>]
		]).

-spec binary_join([binary()], binary()) -> binary().
binary_join([], _Sep) ->
	<<>>;
binary_join([Part], _Sep) ->
	Part;
binary_join(List, Sep) ->
	lists:foldr(fun (A, B) ->
		if
			bit_size(B) > 0 -> <<A/binary, Sep/binary, B/binary>>;
			true -> A
		end
	end, <<>>, List).

lasttail([T|[]]) -> T;
lasttail([_|TT]) -> lasttail(TT).

eightball() ->
	case file:read_file("eightball.txt") of
		{ok, B} ->
			R = binary:split(B, <<"\n">>, [global]);
		_ ->
			R = [
				"It is certain",
				"It is decidedly so",
		        	"Without a doubt",
				"Yes - definitely",
				"You may rely on it",
				"As I see it, yes",
				"Most likely",
				"Outlook good",
				"Signs point to yes",
				"Yes",
				"Don't count on it",
				"My reply is no",
				"My sources say no",
				"Outlook not so good",
				"Very doubtful"
			]
	end,
	lists:nth(random:uniform(length(R)), R).

addeightball(TRaw) ->
	T = case lists:member(binary:at(TRaw, byte_size(TRaw)-1), [$., $?, $!]) of
		true -> TRaw;
		false -> <<TRaw/binary, ".">>
	end,
	case file:read_file("eightball.txt") of
		{ok, B} ->
			R = binary:split(B, <<"\n">>, [global]),
			case lists:member(T, R) of
				true -> "That reply already exists!";
				false ->
					case file:write_file("eightball.txt", <<B/binary, T/binary, "\n">>) of
						ok -> "Added.";
						{error, T} -> "Error writing file."
					end
			end;
		_ ->
			"Error reading file."
	end.
