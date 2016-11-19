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
			[<<"\\-">>, <<"\\\\-">>],
			[<<"\\|">>, <<"\\\\|">>]
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
			[<<"\\*">>, <<".*">>],
			[<<"\\|">>, <<"\\\\|">>]
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

droplast(T) -> lists:droplast(T).

lasttail([T|[]]) -> T;
lasttail([_|TT]) -> lasttail(TT).

parse_htmlentities(Binary) -> parse_htmlentities(Binary, <<>>).  

parse_htmlentities(<<	      >>, X) -> X;  
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
	Num = integer_to_binary(T),
	parse_htmlentities(<<"&#", Num/binary, ";", B/binary>>, X).

charents() ->
	[
		{<<"amp" >>, $&},
		{<<"gt"  >>, $>},
		{<<"lt"  >>, $<},
		{<<"quot">>, $"},
		{<<"apos">>, $'},

		{<<"bull">>, 8226},
		{<<"ndash">>, 8211},

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

% os:cmd() returns non-strings that re:replace can't handle, this fixes them
safe_os_cmd(String) ->
	lists:flatmap(fun
			(T) when T < 256 -> [T];
			(T) -> binary_to_list(<<T/utf8>>)
		end, unicode_os_cmd(String)).

unicode_os_putenv(Key, Value) ->
	os:putenv(unicode(Key), unicode(Value)).

unicode_os_cmd(String) ->
	os:cmd(unicode(String)).

unicode(String) ->
	Bin = if
		is_binary(String) -> String;
		is_list(String) -> list_to_binary(String)
	end,
	utf8_chars(Bin).

utf8_chars(<<A/utf8, B/binary>>) -> [A | utf8_chars(B)];
utf8_chars(<<>>) -> [].

whois(Nick) ->
	LNick = string:to_lower(Nick),
	case case config:get_value(temp, [?MODULE, whois_cache, LNick]) of
		'$none' -> query;
		{Timestamp, D} ->
			ThenSecs = calendar:datetime_to_gregorian_seconds(Timestamp),
			NowSecs = calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(os:timestamp())),
			if
				NowSecs - ThenSecs < 300 -> D;
				true -> query
			end
	end of
		query ->
			case whereis(bot) == self() of
				true ->
					whois0(Nick);
				false ->
					bot ! {request_execute, {self(), fun() -> whois0(Nick) end}},
					receive
						{execute_done, Result} ->
							Result
					end
			end;
		Data -> Data
	end.

whois0(Nick) ->
	core ! {raw, ["WHOIS ", Nick]},
	Data = receive_whois(#{ % defaults for the optional fields
		operator => false,
		cloak => false,
		registered => false,
		ssl => false,
		fingerprint => none
	}),
	case Data of
		no_such_nick -> ok;
		_ ->
			insert_whois(Nick, Data)
	end,
	Data.

insert_whois(Nick, Data) ->
	Timestamp = calendar:now_to_universal_time(os:timestamp()),
	config:set_value(temp, [?MODULE, whois_cache, string:to_lower(Nick)], {Timestamp, Data}).

receive_whois(Map) ->
	receive
		{irc, {numeric, {{err,no_such_nick}, _}}} ->
			receive_whois(no_such_nick); % next reply is an RPL_ENDOFWHOIS which just returns, catch that too to avoid spamming logs
		{irc, {numeric, {{rpl,whois_user}, [_,N,U,H,_|Real]}}} ->
			R = string:join([tl(hd(Real)) | tl(Real)], " "),
			receive_whois(Map#{nick=>N, user=>U, host=>H, real=>R});
		{irc, {numeric, {{rpl,whois_channels}, [_, _ | Channels]}}} ->
			receive_whois(Map#{channels=>[tl(hd(Channels)) | tl(Channels)]});
		{irc, {numeric, {{rpl,whois_server}, [_, _, Server | Tagline]}}} ->
			receive_whois(Map#{server=>Server, server_tagline=>string:join([tl(hd(Tagline))|tl(Tagline)], " ")});
		{irc, {numeric, {{rpl,whois_operator}, _}}} ->
			receive_whois(Map#{operator=>true});
		{irc, {numeric, {{unknown, 338}, [_, TrueHost, ":has", "cloak"]}}} ->
			receive_whois(Map#{cloak=>TrueHost});
		{irc, {numeric, {{unknown, 338}, [_, _, TrueHost, ":has", "cloak"]}}} ->
			receive_whois(Map#{cloak=>TrueHost});
		{irc, {numeric, {{unknown, 330}, [_, _, Nickserv, ":is", "logged", "in", "as"]}}} -> % SorceryNet
			receive_whois(Map#{nickserv=>Nickserv});
		{irc, {numeric, {{unknown, 307}, [_, Nick, ":has", "identified", "for", "this", "nick"]}}} -> % Rizon
			receive_whois(Map#{nickserv=>Nick});
		{irc, {numeric, {{unknown, 307}, _}}} ->
			receive_whois(Map#{registered=>true});
		{irc, {numeric, {{unknown, 671}, _}}} ->
			receive_whois(Map#{ssl=>true});
		{irc, {numeric, {{unknown, 276}, [_, _, ":has", "client", "certificate", "fingerprint", Fingerprint]}}} ->
			receive_whois(Map#{fingerprint => Fingerprint});
		{irc, {numeric, {{rpl,whois_idle}, [_, _, Idle, Signon, ":seconds", "idle,", "signon", "time"]}}} ->
			receive_whois(Map#{idle=>Idle, signon=>Signon});
		{irc, {numeric, {{rpl,end_of_whois}, _}}} ->
			Map
	after
		2000 -> Map
	end.

call_or(Mod, Func, Args, Or) ->
	case lists:member({Func,length(Args)}, Mod:module_info(exports)) of
		true -> apply(Mod, Func, Args);
		false -> Or
	end.

fix_utf8(Str) -> lists:reverse(fix_utf8(Str, [])).
fix_utf8([A|R], O) when is_integer(A) andalso A > 128 -> fix_utf8(R, [binary_to_list(<<A/utf8>>) | O]);
fix_utf8([A|R], O) when is_list(A) -> fix_utf8(R, [fix_utf8(A) | O]);
fix_utf8([A|R], O) -> fix_utf8(R, [A|O]);
fix_utf8([], O) -> O.

bin_to_lower(Bin) -> bin_to_lower(Bin, <<>>).

bin_to_lower(<<A/utf8, Rst/binary>>, X) when $A =< A andalso A =< $Z ->
	NewA = A + $a - $A,
	bin_to_lower(Rst, <<X/binary, NewA/utf8>>);
bin_to_lower(<<A/utf8, Rst/binary>>, X) -> bin_to_lower(Rst, <<X/binary, A/utf8>>);
bin_to_lower(<<>>, X) -> X.


pick_rand([X]) -> X;
pick_rand(List) ->
	if
		length(List) > 100 ->
			[Half] = dice3:get_n_m(1, 2),
			pick_rand(element(Half, lists:split(length(List) bsr 1, List)));
		true ->
			[N] = dice3:get_n_m(1, length(List)),
			lists:nth(N, List)
	end.

simple_pick(Lst) ->
	lists:nth(random:uniform(length(Lst)), Lst).

floor(X) when X < 0 ->
	T = trunc(X),
	case X - T == 0 of
		true -> T;
		false -> T - 1
	end;
floor(X) ->
	trunc(X).

ceil(X) -> -floor(-X).


format_date({Y, M, D}) -> io_lib:format("~b-~2..0b-~2..0b", [Y, M, D]).
format_time({H, M, S}) -> io_lib:format("~2..0b:~2..0b:~2..0b", [H, M, S]).
format_datetime({Date,Time}) -> io_lib:format("~s ~s", [format_date(Date), format_time(Time)]).


bin2hex(Bin) -> bin2hex(Bin, <<>>).

bin2hex(<<>>, Out) -> Out;
bin2hex(<<A,In/binary>>, Out) ->
	Ahex = list_to_binary(io_lib:format("~2.16.0b",[A])),
	bin2hex(In, <<Out/binary, Ahex/binary>>).
