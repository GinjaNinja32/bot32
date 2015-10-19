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

droplast(T) -> lists:reverse(tl(lists:reverse(T))).

lasttail([T|[]]) -> T;
lasttail([_|TT]) -> lasttail(TT).

eightball() ->
	case file:read_file("eightball.txt") of
		{ok, B} ->
			R = lists:filter(fun(<<>>)->false; (_)->true end, binary:split(B, <<"\n">>, [global]));
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

parse_htmlentities(Binary) -> parse_htmlentities(Binary, <<>>).  

parse_htmlentities(<<              >>, X) -> X;  
%parse_htmlentities(<<"&quot;", B/binary>>, X) -> parse_htmlentities(B, <<X/binary, "\"">>);  
%parse_htmlentities(<<"&amp;",  B/binary>>, X) -> parse_htmlentities(B, <<X/binary, "&">>);  
%parse_htmlentities(<<"&apos;", B/binary>>, X) -> parse_htmlentities(B, <<X/binary, "'">>);  
%parse_htmlentities(<<"&lt;",   B/binary>>, X) -> parse_htmlentities(B, <<X/binary, "<">>);  
%parse_htmlentities(<<"&gt;",   B/binary>>, X) -> parse_htmlentities(B, <<X/binary, ">">>);  
parse_htmlentities(<<"&#", B/binary>>, X) ->  
	case read_integer(B, 0) of  
		{I, Rest} -> parse_htmlentities(Rest, <<X/binary, I/utf8>>);  
		false -> parse_htmlentities(B, <<X/binary, "&#">>)  
	end;  
parse_htmlentities(<<"&", B/binary>>, X) ->
	case read_ident(B, <<"">>) of
		{I, Rest} -> parse_htmlentities(Rest, <<X/binary, I/binary>>);
		false -> parse_htmlentities(B, <<X/binary, "&">>)
	end;
parse_htmlentities(<<T/utf8, B/binary>>, X) -> parse_htmlentities(B, <<X/binary, T/utf8>>);
parse_htmlentities(<<T, B/binary>>, X) ->
	logging:log(error, "UTIL", "T-value is ~p", [T]),
	parse_htmlentities(B, X).

charents() ->
	[
		{<<"amp" >>, $&},
		{<<"gt"  >>, $>},
		{<<"lt"  >>, $<},
		{<<"quot">>, $"},
		{<<"apos">>, $'},
		{<<"acute">>, 180},
		{<<"cedil">>, 184},
		{<<"circ">>, 710},
		{<<"macr">>, 175},
		{<<"middot">>, 183},
		{<<"tilde">>, 732},
		{<<"uml">>, 168},

		{<<"Aacute">>, 193},
		{<<"aacute">>, 225},
		{<<"Acirc">>, 194},
		{<<"AElig">>, 198},
		{<<"aelig">>, 230},
		{<<"Agrave">>, 192},
		{<<"agrave">>, 224},
		{<<"Aring">>, 197},
		{<<"aring">>, 229},
		{<<"Atilde">>, 195},
		{<<"atilde">>, 227},
		{<<"Auml">>, 196},
		{<<"auml">>, 228},
		{<<"Ccedil">>, 199},
                {<<"ccedil">>, 231},
                {<<"Eacute">>, 201},
                {<<"eacute">>, 233},
                {<<"Ecirc">>, 202},
                {<<"ecirc">>, 234},
                {<<"Egrave">>, 200},
		{<<"egrave">>, 232},
                {<<"ETH">>, 208},
                {<<"eth">>, 240},
                {<<"Euml">>, 203},
                {<<"euml">>, 235},
                {<<"Iacute">>, 205},
                {<<"iacute">>, 237},
                {<<"Icirc">>, 206},
                {<<"icirc">>, 238},
                {<<"Igrave">>, 204},
                {<<"igrave">>, 236},
                {<<"Iuml">>, 207},
                {<<"iuml">>, 239},
                {<<"Ntilde">>, 209},
                {<<"ntilde">>, 241},
                {<<"Oacute">>, 211},
                {<<"oacute">>, 243},
                {<<"Ocirc">>, 212},
                {<<"ocirc">>, 244},
                {<<"OElig">>, 338},
                {<<"oelig">>, 339},
                {<<"Ograve">>, 210},
                {<<"ograve">>, 242},
                {<<"Oslash">>, 216},
                {<<"oslash">>, 248},
                {<<"Otilde">>, 213},
                {<<"otilde">>, 245},
                {<<"Ouml">>, 214},
                {<<"ouml">>, 246},
                {<<"Scaron">>, 352},
                {<<"scaron">>, 353},
                {<<"szlig">>, 223},
                {<<"THORN">>, 222},
                {<<"thorn">>, 254},
                {<<"Uacute">>, 218},
                {<<"uacute">>, 250},
                {<<"Ucirc">>, 219},
                {<<"ucirc">>, 251},
                {<<"Ugrave">>, 217},
                {<<"ugrave">>, 249},
                {<<"Uuml">>, 220},
                {<<"uuml">>, 252},
                {<<"Yacute">>, 221},
                {<<"yacute">>, 253},
                {<<"Yuml">>, 376},
                {<<"yuml">>, 255}
	].

read_ident(<<>>, _) -> false;
read_ident(<<";", Rest/binary>>, N) ->
	case lists:keyfind(N, 1, charents()) of
		{N, V} when is_integer(V) -> {<<V/utf8>>, Rest};
		{N, V} when is_binary(V) -> {V, Rest};
		false -> false
	end;
read_ident(<<A/utf8, B/binary>>, N) -> read_ident(B, <<N/binary, A/utf8>>).

read_integer(<<>>, _) -> false;  
read_integer(<<"x", Rest/binary>>, 0) -> read_hex(Rest, 0);  
read_integer(<<";", Rest/binary>>, N) -> {N,Rest};  
read_integer(<<A/utf8, B/binary>>, N) ->  
	if  
		$0 =< A andalso A =< $9 -> read_integer(B, N*10 + (A-$0));
		true -> false
	end.   
       
read_hex(<<>>, _) -> false;
read_hex(<<";", Rest/binary>>, N) -> {N,Rest};
read_hex(<<A/utf8, B/binary>>, N) ->
	if
		$0 =< A andalso A =< $9 -> read_hex(B, N*16 + (A-$0));
		$a =< A andalso A =< $f -> read_hex(B, N*16 + (10+A-$a));
		$A =< A andalso A =< $A -> read_hex(B, N*16 + (10+A-$A));
		true -> false
	end.   

count(Func, List) ->
	lists:foldl(fun(T, N) ->
			case Func(T) of
				true -> N+1;
				_ -> N
			end
		end, 0, List).

s(N) -> s(N, "s").

s(1,_) -> "";
s(_,T) -> T.

waitfor(Ident) ->
	case whereis(Ident) of
		undefined ->
			timer:sleep(100),
			waitfor(Ident);
		_ -> ok
	end.
waitfor_gone(Ident) ->
	case whereis(Ident) of
		undefined -> ok;
		_ ->
			timer:sleep(100),
			waitfor_gone(Ident)
	end.
