-module(google).
-compile(export_all).

get_commands() ->
	[
		{"google", fun google/1, user}
	].

google(#{reply:=RT, ping:=P, params:=[]}) -> {irc, {msg, {RT, [P, "Provide a term to search for."]}}};
google(#{reply:=RT, ping:=P, params:=Params}) ->
	case lists:all(fun(T) -> lists:member(T,"0123456789") end, hd(Params)) of
		true ->
			N = list_to_integer(hd(Params)) - 1,
			SearchTerm = string:join(tl(Params), " ");
		_ ->
			N = 0,
			SearchTerm = string:join(Params, " ")
	end,
	{Term, Display} = case lists:splitwith(fun(T) -> T /= $% end, SearchTerm) of
		{_, []} -> {SearchTerm, SearchTerm};
		{A, [_|B]} -> {A ++ B, string:strip(B)}
	end,
	case SearchTerm of
		[] -> {irc, {msg, {RT, [P, "Provide a term to search for."]}}};
		_ -> spawn(?MODULE, search, [RT, P, N, Term, Display]), ok
	end.

search(RT, P, N, Term, Display) ->
	case case gen_tcp:connect("ajax.googleapis.com", 80, [{active, false}, {packet, http}]) of
		{ok, Sock} ->
			gen_tcp:send(Sock, "GET " ++ mkurl(N, Term) ++ " HTTP/1.1\r\nHost: ajax.googleapis.com\r\n\r\n"),
			case gen_tcp:recv(Sock, 0) of
				{ok, {http_response, _, 200, "OK"}} ->
					inet:setopts(Sock, [{packet, httph}]),
					Dict = read_headers(Sock),
					case orddict:find('Content-Length', Dict) of
						{ok, Value} ->
							inet:setopts(Sock, [{packet, raw}]),
							case gen_tcp:recv(Sock, list_to_integer(Value), 5000) of
								{ok, Data} -> json_handle(RT, P, N, Term, Data, Display);
								T -> common:debug("debug", "recv gave ~p", [T]), error
							end;
						error ->
							case orddict:find('Transfer-Encoding', Dict) of
								{ok, "chunked"} ->
									inet:setopts(Sock, [{packet, raw}]),
									read_chunked(RT, P, N, Term, Sock, Display);
								_ -> logging:log(error, "GOOGLE", "no Content-Length and unknown or not present Transfer-Encoding"), error
							end
					end;
				T -> common:debug("error", "recv gave ~p", [T]), error
			end,
			gen_tcp:close(Sock);
		T -> common:debug("error", "connect gave ~p", [T]), error
	end of
		error -> core ! {irc, {msg, {RT, [P, "Error in Google search."]}}};
		_ -> ok
	end.

read_chunked(RT, P, N, Term, Sock, Display) ->
	case read_chunked(Sock, []) of
		error -> error;
		Data -> json_handle(RT, P, N, Term, Data, Display)
	end.

read_chunked(Sock, Data) ->
	inet:setopts(Sock, [{packet, line}]),
	case gen_tcp:recv(Sock, 0, 2000) of
		{ok, Line} ->
			case list_to_integer(lists:takewhile(fun(T)->T>32 end, Line), 16) of
				0 -> Data;
				ChunkSize ->
					inet:setopts(Sock, [{packet, raw}]),
					case gen_tcp:recv(Sock, ChunkSize, 2000) of
						{ok, NewData} ->
							gen_tcp:recv(Sock, 2, 2000),
							read_chunked(Sock, Data ++ NewData);
						T -> common:debug("error", "recv gave ~p", [T]), error
					end
			end;
		T -> common:debug("error", "recv gave ~p", [T]), error
	end.

mkurl(N, Term) ->
	"/ajax/services/search/web?q=" ++ http_uri:encode(Term) ++ "&v=1.0&start=" ++ integer_to_list(4 * (N div 4)) ++ "&rsz=small&safe=active&hl=en".

json_handle(RT, P, N, Term, RawJSON, Display) ->
	case catch mochijson:decode(RawJSON) of
		{'EXIT', T} ->
			logging:log(error, "GOOGLE", "Error in mochijson: ~p", [T]),
			ok;
		JSON ->
			Reply = case traverse_json(JSON, [struct, "responseData", struct, "results", array]) of
				[] -> "No results found.";
				List ->
					Result = lists:nth(1 + (N rem 4), List),
					URL = traverse_json(Result, [struct, "unescapedUrl"]),
					Title = traverse_json(Result, [struct, "titleNoFormatting"]),
					FixedTitle = fix(Title),
					[$(, integer_to_list(N+1), $), $ , Display, ": ", URL, " - ", FixedTitle]
			end,
			core ! {irc, {msg, {RT, [P, Reply]}}}
	end.

fix(Title) -> util:fix_utf8(Title).

traverse_json(JSON, []) -> JSON;
traverse_json({X,T}, [X|Path]) -> traverse_json(T, Path);
traverse_json(Dict, [Key|Path]) ->
	case lists:keyfind(Key, 1, Dict) of
		false -> error;
		{_, V} -> traverse_json(V, Path)
	end.

read_headers(Sock) -> read_headers(Sock, orddict:new()).
read_headers(Sock, Dict) ->
	case gen_tcp:recv(Sock, 0, 2000) of
		{ok, {http_header, _, Key, _, Val}} -> read_headers(Sock, orddict:store(Key, Val, Dict));
		{ok, http_eoh} -> Dict;
		{ok, T} -> {error, {unexpected, T}};
		{error, T} -> {error, T}
	end.
