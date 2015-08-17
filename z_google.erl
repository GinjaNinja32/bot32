-module(z_google).
-compile(export_all).

get_commands() ->
	[
		{"google", fun google/5, user}
	].

google(_, RT, P, Params, _) ->
	SearchTerm = string:join(Params, " "),
	spawn(?MODULE, search, [RT, P, SearchTerm]),
	ok.

search(RT, P, Term) ->
	case case gen_tcp:connect("ajax.googleapis.com", 80, [{active, false}, {packet, http}]) of
		{ok, Sock} ->
			gen_tcp:send(Sock, "GET " ++ mkurl(Term) ++ " HTTP/1.1\r\nHost: ajax.googleapis.com\r\n\r\n"),
			case gen_tcp:recv(Sock, 0) of
				{ok, {http_response, _, 200, "OK"}} ->
					inet:setopts(Sock, [{packet, httph}]),
					Dict = read_headers(Sock),
					case orddict:find('Content-Length', Dict) of
						{ok, Value} ->
							inet:setopts(Sock, [{packet, raw}]),
							case gen_tcp:recv(Sock, list_to_integer(Value), 5000) of
								{ok, Data} -> json_handle(RT, P, Term, Data);
								T -> common:debug("debug", "recv gave ~p", [T]), error
							end;
						error ->
							case orddict:find('Transfer-Encoding', Dict) of
								{ok, "chunked"} ->
									inet:setopts(Sock, [{packet, raw}]),
									read_chunked(RT, P, Term, Sock);
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

read_chunked(RT, P, Term, Sock) ->
	case read_chunked(Sock, []) of
		error -> error;
		Data -> json_handle(RT, P, Term, Data)
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

mkurl(Term) ->
	"/ajax/services/search/web?q=" ++ http_uri:encode(Term) ++ "&v=1.0&start=1&rsz=small&safe=active&hl=en".

json_handle(RT, P, Term, RawJSON) ->
%	common:debug("debug", "json is ~s", [RawJSON]),
	case catch mochijson:decode(RawJSON) of
		{'EXIT', T} ->
			logging:log(error, "GOOGLE", "Error in mochijson: ~p", [T]),
			ok;
		JSON ->
			FirstResult = hd(traverse_json(JSON, [struct, "responseData", struct, "results", array])),
			URL = traverse_json(FirstResult, [struct, "url"]),
			Title = traverse_json(FirstResult, [struct, "titleNoFormatting"]),
			core ! {irc, {msg, {RT, [P, Term, ": ", URL, " - ", Title]}}}
%			common:debug("debug", "google returned ~p", [Data])
	end.

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
