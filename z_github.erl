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
	Channels = case file:consult("github_announce.crl") of
		{ok, Terms} -> Terms;
		{error, T} -> ["#bot32-test"];
		_ -> ["#bot32-test"]
	end,
	spawn(?MODULE, init, [Channels]),
	T.

deinitialise(T) ->
	case whereis(?MODULE) of
		undefined -> ok;
		Pid -> Pid ! stop
	end,
	T.

init(Channels) ->
	case gen_tcp:listen(8080, [binary, {packet, http}, {active, false}, {reuseaddr, true}]) of
		{ok, SvrSock} ->
			logging:log(info, "GITHUB", "Starting loop."),
			register(?MODULE, self()),
			loop(SvrSock, Channels),
			gen_tcp:close(SvrSock),
			logging:log(info, "GITHUB", "Ending loop.");
		{error, Reason} ->
			logging:log(error, "GITHUB", "Failed to open listen socket: ~p", [Reason])
	end.

loop(SvrSock, Channels) ->
	case receive
		stop -> stop
	after ?Timer ->
		case gen_tcp:accept(SvrSock, ?Timer) of
			{ok, Socket} -> handle_sock(Socket, Channels), ok;
			{error, timeout} -> ok;
			{error, X} -> {error, X}
		end
	end of
		ok -> loop(SvrSock, Channels);
		stop -> ok;
		{error, E} -> logging:log(error, "GITHUB", "error: ~p", [E]);
		E -> logging:log(error, "GITHUB", "Unknown status ~p", [E])
	end.

handle_sock(Socket, Channels) ->
	case gen_tcp:recv(Socket, 0, 2000) of
		{ok, {http_request, 'POST', _, _}} ->
			inet:setopts(Socket, [{packet, httph}]),
			Dict = read_headers(Socket),
			case case orddict:find('Content-Length', Dict) of
				{ok, Value} ->
					inet:setopts(Socket, [{packet, raw}]),
					read_content(Socket, list_to_integer(Value));
				error -> <<"">>
			end of
				<<"">> -> [];
				{error, T} -> logging:log(error, "GITHUB", "Error: ~p", [T]);
				Content -> decode_content(Content, Channels)
			end,
			gen_tcp:send(Socket, "HTTP/1.1 204 No Content\r\n\r\n"),
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

read_content(Socket, Length) ->
	case gen_tcp:recv(Socket, Length, 10000) of
		{ok, Data} -> Data;
		{error, T} -> {error, T}
	end.

decode_content(Content, Channels) ->
	case catch mochijson:decode(Content) of
		{'EXIT', T} -> logging:log(error, "GITHUB", "Error in mochijson: ~p", [T]);
		NewC ->
			file:write_file("json.crl", io_lib:format("~p",[NewC])),
			handle_decoded(NewC, Channels)
	end.

handle_decoded(JSON, Channels) ->
	Message = case traverse_json(JSON, [struct, "action"]) of
		"opened" ->
			create_message(JSON, "[~s] ~s opened pull request #~b: ~s (~s...~s) ~s", [
					[struct, "pull_request", struct, "base", struct, "repo", struct, "name"],
					[struct, "sender", struct, "login"],
					[struct, "pull_request", struct, "number"],
					[struct, "pull_request", struct, "title"],
					[struct, "pull_request", struct, "base", struct, "label"],
					[struct, "pull_request", struct, "head", struct, "label"],
					[struct, "pull_request", struct, "html_url"]
				]);
		"reopened" ->
			create_message(JSON, "[~s] ~s reopened pull request #~b: ~s (~s...~s) ~s", [
					[struct, "pull_request", struct, "base", struct, "repo", struct, "name"],
					[struct, "sender", struct, "login"],
					[struct, "pull_request", struct, "number"],
					[struct, "pull_request", struct, "title"],
					[struct, "pull_request", struct, "base", struct, "label"],
					[struct, "pull_request", struct, "head", struct, "label"],
					[struct, "pull_request", struct, "html_url"]
				]);
		"closed" ->
			create_message(JSON, "[~s] ~s closed pull request #~b: ~s (~s...~s) ~s", [
					[struct, "pull_request", struct, "base", struct, "repo", struct, "name"],
					[struct, "sender", struct, "login"],
					[struct, "pull_request", struct, "number"],
					[struct, "pull_request", struct, "title"],
					[struct, "pull_request", struct, "base", struct, "label"],
					[struct, "pull_request", struct, "head", struct, "label"],
					[struct, "pull_request", struct, "html_url"]
				]);
		error ->
			case lists:map(fun(T) -> traverse_json(JSON, [struct, T]) end, ["created", "deleted", "forced"]) of
				[true, false, _] ->
					create_message(JSON, "[~s] ~s created ~s at ~s: ~s", [
							[struct, "repository", struct, "name"],
							[struct, "sender", struct, "login"],
							[struct, "ref"],
							{hash, [struct, "after"]},
							[struct, "compare"]
						]);
				[false, true, _] ->
					create_message(JSON, "[~s] ~s deleted ~s at ~s", [
							[struct, "repository", struct, "name"],
							[struct, "sender", struct, "login"],
							[struct, "ref"],
							{hash, [struct, "before"]}
						]);
				[false, false, Force] ->
					Pushed = if Force -> "force-pushed"; true -> "pushed" end,
					create_message(JSON, "[~s] ~s ~s ~b commits to ~s (from ~s to ~s): ~s", [
							[struct, "repository", struct, "name"],
							[struct, "sender", struct, "login"],
							{Pushed},
							{length, [struct, "commits", array]},
							[struct, "ref"],
							{hash, [struct, "before"]},
							{hash, [struct, "after"]},
							[struct, "compare"]
						]);
				[error, error, error] -> "";
				[_, _, _] -> "???"
			end;
		_ -> "???"
	end,
	case case Message of
		"" -> false;
		"???" -> file:write_file("json_err.crl", io_lib:format("~p", [JSON])), true;
		_ -> true
	end of
		true -> lists:foreach(fun(T) -> core ! {irc, {msg, {T, Message}}} end, Channels);
		false -> ok
	end.

create_message(JSON, String, FormatJsonPaths) ->
	io_lib:format(String, lists:map(fun
			({T}) -> T;
			({hash,T}) -> {X,_} = lists:split(12, traverse_json(JSON, T)), X;
			({length,T}) -> length(traverse_json(JSON, T));
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

