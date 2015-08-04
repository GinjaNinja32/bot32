-module(z_github).
-compile(export_all).

-define(Timer, 100).

waitfor_gone(Ident) ->
	case whereis(Ident) of
		undefined -> ok;
		_ -> timer:sleep(100), waitfor_gone(Ident)
	end.

get_commands() -> [].
initialise(T) ->
	case whereis(?MODULE) of
		undefined -> ok;
		Pid -> Pid ! stop, waitfor_gone(?MODULE)
	end,
	case file:consult("github_announce.crl") of
		{ok, [Secret,Channels]} -> ok;
		_ ->
			Secret = '*',
			Channels = [{'*', '*', "#bot32-test"}]
	end,
	spawn(?MODULE, init, [Channels, Secret]),
	T.

deinitialise(T) ->
	case whereis(?MODULE) of
		undefined -> ok;
		Pid -> Pid ! stop
	end,
	T.

init(Channels, Secret) ->
	case gen_tcp:listen(8080, [binary, {packet, http}, {active, false}, {reuseaddr, true}]) of
		{ok, SvrSock} ->
			logging:log(info, "GITHUB", "Starting loop."),
			register(?MODULE, self()),
			loop(SvrSock, Channels, Secret),
			gen_tcp:close(SvrSock),
			logging:log(info, "GITHUB", "Ending loop.");
		{error, Reason} ->
			logging:log(error, "GITHUB", "Failed to open listen socket: ~p", [Reason])
	end.

loop(SvrSock, Channels, Secret) ->
	case receive
		stop -> stop
	after ?Timer ->
		case gen_tcp:accept(SvrSock, ?Timer) of
			{ok, Socket} -> handle_sock(Socket, Channels, Secret), ok;
			{error, timeout} -> ok;
			{error, X} -> {error, X}
		end
	end of
		ok -> loop(SvrSock, Channels, Secret);
		stop -> ok;
		{error, E} -> logging:log(error, "GITHUB", "error: ~p", [E]);
		E -> logging:log(error, "GITHUB", "Unknown status ~p", [E])
	end.

handle_sock(Socket, Channels, Secret) ->
	case gen_tcp:recv(Socket, 0, 2000) of
		{ok, {http_request, 'POST', A, B}} ->
			inet:setopts(Socket, [{packet, httph}]),
			Dict = read_headers(Socket),
			case orddict:find("X-Hub-Signature", Dict) of
				{ok, Signature} ->
					case case case orddict:find('Content-Length', Dict) of
						{ok, Value} ->
							inet:setopts(Socket, [{packet, raw}]),
							read_content(Socket, list_to_integer(Value), Signature, Secret);
						error -> <<"">>
					end of
						sigerr -> error;
						<<"">> -> ok;
						{error, T} -> logging:log(error, "GITHUB", "Error: ~p", [T]);
						Content -> decode_content(Content, Channels), ok
					end of
						ok ->
							gen_tcp:send(Socket, "HTTP/1.1 204 No Content\r\n\r\n");
						error ->
							common:debug("GITHUB", "HTTP request with incorrect signature ~p ~p ~p", [A, B, Dict]),
							gen_tcp:send(Socket, "HTTP/1.1 403 Forbidden\r\n\r\n")
					end;
				error -> 
					common:debug("GITHUB", "HTTP request with no signature ~p ~p ~p", [A, B, Dict]),
					gen_tcp:send(Socket, "HTTP/1.1 403 Forbidden\r\n\r\n")
			end,
			gen_tcp:close(Socket);
		{ok, T} ->
			common:debug("GITHUB", "HTTP request ~p", [T]),
			gen_tcp:send(Socket, "HTTP/1.1 204 No Content\r\n\r\n"),
			gen_tcp:close(Socket);
		{error, T} ->
			common:debug("GITHUB", "HTTP error: ~p", [T])
	end.

read_headers(Socket) -> read_headers(Socket, orddict:new()).
read_headers(Socket, Dict) ->
	case gen_tcp:recv(Socket, 0, 2000) of
		{ok, {http_header, _,Key,_,Val}} ->
			read_headers(Socket, orddict:store(Key, Val, Dict));
		{ok, http_eoh} ->
			Dict;
		{ok, T} ->
			{error, {unexpected, T}};
		{error, T} ->
			{error, T}
	end.

read_content(Socket, Length, Signature, Secret) ->
	case gen_tcp:recv(Socket, Length, 10000) of
		{ok, Data} ->
			case Secret of
				'*' -> Data;
				_ ->
					% crypto:hmac returns a binary
					% github inserts 'sha1=' in front of the signature
					RealSignature = list_to_binary(tl(tl(tl(tl(tl(Signature)))))),
					case hexit(crypto:hmac(sha, Secret, Data), <<>>) of
						RealSignature -> Data;
						X ->
							io:fwrite("got ~p, expected ~p~n", [X, RealSignature]),
							sigerr
					end
			end;
		{error, T} -> {error, T}
	end.

hexit(<<>>, Out) -> Out;
hexit(<<A,In/binary>>, Out) ->
	case io_lib:format("~.16b",[A]) of
		[X] when length(X) == 2 -> Bin = list_to_binary(X);
		[X] -> Bin = list_to_binary("0" ++ X)
	end,
	hexit(In, <<Out/binary, Bin/binary>>).

decode_content(Content, Channels) ->
	case catch mochijson:decode(Content) of
		{'EXIT', T} -> logging:log(error, "GITHUB", "Error in mochijson: ~p", [T]);
		NewC ->
			file:write_file("json.crl", io_lib:format("~p",[NewC])),
			handle_decoded(NewC, Channels)
	end.

handle_decoded(JSON, Channels) ->
	Messages = case {
				traverse_json(JSON, [struct, "action"]),
				traverse_json(JSON, [struct, "pull_request"]),
				traverse_json(JSON, [struct, "issue"])
			} of
		{"opened", _, error} ->
			RepoStruct = traverse_json(JSON, [struct, "pull_request", struct, "base", struct, "repo"]),
			[create_message(JSON, "[~s] ~s opened pull request #~b: ~s (~s...~s) ~s", [
					{reponame, [struct, "pull_request", struct, "base", struct, "repo"]},
					[struct, "sender", struct, "login"],
					[struct, "pull_request", struct, "number"],
					[struct, "pull_request", struct, "title"],
					[struct, "pull_request", struct, "base", struct, "label"],
					[struct, "pull_request", struct, "head", struct, "label"],
					[struct, "pull_request", struct, "html_url"]
				])];
		{"reopened", _, error} ->
			RepoStruct = traverse_json(JSON, [struct, "pull_request", struct, "base", struct, "repo"]),
			[create_message(JSON, "[~s] ~s reopened pull request #~b: ~s (~s...~s) ~s", [
					{reponame, [struct, "pull_request", struct, "base", struct, "repo"]},
					[struct, "sender", struct, "login"],
					[struct, "pull_request", struct, "number"],
					[struct, "pull_request", struct, "title"],
					[struct, "pull_request", struct, "base", struct, "label"],
					[struct, "pull_request", struct, "head", struct, "label"],
					[struct, "pull_request", struct, "html_url"]
				])];
		{"closed", _, error} ->
			RepoStruct = traverse_json(JSON, [struct, "pull_request", struct, "base", struct, "repo"]),
			[create_message(JSON, "[~s] ~s closed pull request #~b: ~s (~s...~s) ~s", [
					{reponame, [struct, "pull_request", struct, "base", struct, "repo"]},
					[struct, "sender", struct, "login"],
					[struct, "pull_request", struct, "number"],
					[struct, "pull_request", struct, "title"],
					[struct, "pull_request", struct, "base", struct, "label"],
					[struct, "pull_request", struct, "head", struct, "label"],
					[struct, "pull_request", struct, "html_url"]
				])];
		{"opened", error, _} ->
			RepoStruct = traverse_json(JSON, [struct, "issue", struct, "repository"]),
			[create_message(JSON, "[~s] ~s opened issue #~b: ~s ~s", [
					{reponame, [struct, "repository"]},
					[struct, "sender", struct, "login"],
					[struct, "issue", struct, "number"],
					[struct, "issue", struct, "title"],
					[struct, "issue", struct, "html_url"]
				])];
		{"reopened", error, _} ->
			RepoStruct = traverse_json(JSON, [struct, "issue", struct, "repository"]),
			[create_message(JSON, "[~s] ~s reopened issue #~b: ~s ~s", [
					{reponame, [struct, "repository"]},
					[struct, "sender", struct, "login"],
					[struct, "issue", struct, "number"],
					[struct, "issue", struct, "title"],
					[struct, "issue", struct, "html_url"]
				])];
		{"closed", error, _} ->
			RepoStruct = traverse_json(JSON, [struct, "issue", struct, "repository"]),
			[create_message(JSON, "[~s] ~s closed issue #~b: ~s ~s", [
					{reponame, [struct, "repository"]},
					[struct, "sender", struct, "login"],
					[struct, "issue", struct, "number"],
					[struct, "issue", struct, "title"],
					[struct, "issue", struct, "html_url"]
				])];
		{error, _, _} ->
			RepoStruct = traverse_json(JSON, [struct, "repository"]),
			case lists:map(fun(T) -> traverse_json(JSON, [struct, T]) end, ["created", "deleted", "forced"]) of
				[true, false, _] ->
					[create_message(JSON, "[~s] ~s created ~s at ~s: ~s", [
							{reponame, [struct, "repository"]},
							[struct, "sender", struct, "login"],
							{ref, [struct, "ref"]},
							{hash, [struct, "after"]},
							[struct, "compare"]
						])];
				[false, true, _] ->
					[create_message(JSON, "[~s] ~s deleted ~s at ~s", [
							{reponame, [struct, "repository"]},
							[struct, "sender", struct, "login"],
							{ref, [struct, "ref"]},
							{hash, [struct, "before"]}
						])];
				[false, false, Force] ->
					Pushed = if Force -> "force-pushed"; true -> "pushed" end,
					PushMsg = create_message(JSON, "[~s] ~s ~s ~b commits to ~s (from ~s to ~s): ~s", [
							{reponame, [struct, "repository"]},
							[struct, "sender", struct, "login"],
							{Pushed},
							{length, [struct, "commits", array]},
							{ref, [struct, "ref"]},
							{hash, [struct, "before"]},
							{hash, [struct, "after"]},
							[struct, "compare"]
						]),
					CommitList = traverse_json(JSON, [struct, "commits", array]),
					ShowList = lists:sublist(CommitList, 3),
					RepoBranch = create_message(JSON, "~s:~s", [
							{reponame, [struct, "repository"]},
							{ref, [struct, "ref"]}
						]),
					ShowMsgs = lists:map(fun(MJSON) ->
						create_message(MJSON, "[~s] ~s ~s: ~s", [
							{RepoBranch},
							{hash, [struct, "id"]},
							[struct, "author", struct, "username"],
							{trunc_newline, [struct, "message"]}
						]) end, ShowList),
					[PushMsg | ShowMsgs];
				[error, error, error] -> [];
				[_, _, _] -> ["???"]
			end;
		_ -> RepoStruct = ["???"]
	end,
	case case Messages of
		[] -> false;
		["???"] -> file:write_file("json_err.crl", io_lib:format("~p", [JSON])), false;
		_ -> true
	end of
		true ->
			UU = traverse_json(RepoStruct, [struct, "owner", struct, "name"]),
			NN = traverse_json(RepoStruct, [struct, "name"]),
			SendChannels = lists:flatmap(fun({U,N,L}) ->
					if
						U == '*' orelse U == UU andalso
						N == '*' orelse N == NN ->
							L;
						true -> []
					end end, Channels),
			lists:foreach(fun(M) -> lists:foreach(fun(T) -> core ! {irc, {msg, {T, M}}} end, SendChannels) end, Messages);
		false -> ok
	end.

create_message(JSON, String, FormatJsonPaths) ->
	io_lib:format(String, lists:map(fun
			({T}) -> T;
			({hash,T}) -> lists:sublist(traverse_json(JSON, T), 8);
			({length,T}) -> length(traverse_json(JSON, T));
			({ref,T}) ->
				case traverse_json(JSON, T) of
					"refs/heads/" ++ Branch -> Branch;
					X -> X
				end;
			({trunc_newline,T}) ->
				case traverse_json(JSON, T) of
					error -> error;
					Lst -> lists:takewhile(fun(X) -> X /= 10 end, Lst)
				end;
			({reponame, RepoStruct}) ->
				case traverse_json(JSON, RepoStruct ++ [struct, "fork"]) of
					false -> traverse_json(JSON, RepoStruct ++ [struct, "name"]);
					_ -> traverse_json(JSON, RepoStruct ++ [struct, "full_name"])
				end;
			(T) -> traverse_json(JSON, T)
		end, FormatJsonPaths)).

traverse_json(error, _) -> error;
traverse_json(JSON, []) -> JSON;
traverse_json({struct,T}, [struct|Path]) -> traverse_json(T, Path);
traverse_json({array,T}, [array|Path]) -> traverse_json(T, Path);
traverse_json(Dict, [Key|Path]) ->
	traverse_json(case lists:keyfind(Key, 1, Dict) of
			false -> error;
			{_,V} -> V
		end, Path).

