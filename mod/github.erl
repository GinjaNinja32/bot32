-module(github).
-compile(export_all).

-include("colordefs.hrl").


-define(URL, ?BLUE).
-define(REPO, ?GREEN).
-define(USER, ?LGREEN).
-define(HASH, ?LGREY).
-define(BRANCH, ?MAGENTA).
-define(TAG, ?LMAGENTA).

-define(Timer, 100).

initialise() ->
	case whereis(?MODULE) of
		undefined -> ok;
		Pid -> Pid ! stop, util:waitfor_gone(?MODULE)
	end,
	spawn(?MODULE, init, []).

deinitialise() ->
	case whereis(?MODULE) of
		undefined -> ok;
		Pid -> Pid ! stop
	end,
	util:waitfor_gone(?MODULE).

init() ->
	case gen_tcp:listen(8080, [binary, {packet, http}, {active, false}, {reuseaddr, true}]) of
		{ok, SvrSock} ->
			logging:log(info, "GITHUB", "Starting loop."),
			register(?MODULE, self()),
			loop(SvrSock),
			gen_tcp:close(SvrSock),
			logging:log(info, "GITHUB", "Ending loop.");
		{error, Reason} ->
			logging:log(error, "GITHUB", "Failed to open listen socket: ~p", [Reason])
	end.

loop(SvrSock) ->
	case receive
		stop -> stop
	after ?Timer ->
		case gen_tcp:accept(SvrSock, ?Timer) of
			{ok, Socket} -> handle_sock(Socket), ok;
			{error, timeout} -> ok;
			{error, X} -> {error, X}
		end
	end of
		ok -> loop(SvrSock);
		stop -> ok;
		{error, E} -> logging:log(error, "GITHUB", "error: ~p", [E]);
		E -> logging:log(error, "GITHUB", "Unknown status ~p", [E])
	end.

fname(Socket) ->
	case inet:peername(Socket) of
		{ok, {Addr, Port}} -> io_lib:format("~p:~p", [Addr, Port]);
		{error, Err} -> io_lib:format("~p", [Err])
	end.

handle_sock(Socket) ->
	case gen_tcp:recv(Socket, 0, 2000) of
		{ok, {http_request, 'POST', A, B}} ->
			inet:setopts(Socket, [{packet, httph}]),
			Dict = read_headers(Socket),
			case orddict:find("X-Hub-Signature", Dict) of
				{ok, Signature} ->
					case case case orddict:find('Content-Length', Dict) of
						{ok, Value} ->
							inet:setopts(Socket, [{packet, raw}]),
							receive_content(Socket, list_to_integer(Value));
						error -> <<"">>
					end of
						<<"">> -> ok;
						{error, T} -> logging:log(error, "GITHUB", "Error: ~p", [T]);
						Content ->
							decode_content(Content, Signature), ok
					end of
						ok ->
							gen_tcp:send(Socket, "HTTP/1.1 204 No Content\r\n\r\n");
						error ->
							logging:log(info, "GITHUB", "HTTP request with incorrect signature from ~s: ~p ~p ~p", [fname(Socket), A, B, Dict]),
							gen_tcp:send(Socket, "HTTP/1.1 403 Forbidden\r\n\r\n")
					end;
				error ->
					logging:log(info, "GITHUB", "HTTP request with no signature from ~s: ~p ~p ~p", [fname(Socket), A, B, Dict]),
					gen_tcp:send(Socket, "HTTP/1.1 403 Forbidden\r\n\r\n")
			end,
			gen_tcp:close(Socket);
		{ok, T} ->
			logging:log(info, "GITHUB", "HTTP request from ~s: ~p", [fname(Socket), T]),
			gen_tcp:send(Socket, "HTTP/1.1 403 Forbidden\r\n\r\n"),
			gen_tcp:close(Socket);
		{error, T} ->
			logging:log(info, "GITHUB", "HTTP error to ~s: ~p", [fname(Socket), T]),
			gen_tcp:close(Socket)
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

receive_content(Socket, Length) ->
	case gen_tcp:recv(Socket, Length, 10000) of
		{ok, Data} -> Data;
		{error, T} -> {error, T}
	end.

hexit(<<>>, Out) -> Out;
hexit(<<A,In/binary>>, Out) ->
	case io_lib:format("~.16b",[A]) of
		[X] when length(X) == 2 -> Bin = list_to_binary(X);
		[X] -> Bin = list_to_binary("0" ++ X)
	end,
	hexit(In, <<Out/binary, Bin/binary>>).

decode_content(Content, Signature) ->
	case catch mochijson:decode(Content) of
		{'EXIT', T} -> logging:log(error, "GITHUB", "Error in mochijson: ~p", [T]);
		JSON ->
			file:write_file("json.crl", io_lib:format("~p",[JSON])),
			case handle_decoded(JSON) of
				{User, Repo, Messages} ->
					case check_signature(Content, Signature, User, Repo) of
						false -> error;
						true ->
							Channels = config:get_value(config, [?MODULE, channels, User, Repo], ["#bot32-test"]),
							lists:foreach(fun(M) ->
								lists:foreach(fun(T) ->
									core ! {irc, {msg, {T, M}}}
								end, Channels)
							end, Messages)
					end;
				_ -> ok
			end
	end.

check_signature(Content, Signature, User, Repo) ->
	case config:get_value(config, [?MODULE, secret, User, Repo]) of
		'$none' ->
			io:fwrite("rejecting unknown user/repo ~p/~p\n", [User, Repo]),
			false;
		'*' -> true;
		Secret ->
			RealSignature = list_to_binary(tl(tl(tl(tl(tl(Signature)))))),
			case hexit(crypto:hmac(sha, Secret, Content), <<>>) of
				RealSignature -> true;
				X ->
					io:fwrite("hashed to ~p, expected ~p\n", [X, RealSignature]),
					false
			end
	end.

handle_decoded(JSON) ->
	Messages = case {
				traverse_json(JSON, [struct, "action"]),
				traverse_json(JSON, [struct, "pull_request"]),
				traverse_json(JSON, [struct, "issue"]),
				traverse_json(JSON, [struct, "state"])
			} of
		{"opened", _, error, _} ->
			RepoStruct = traverse_json(JSON, [struct, "pull_request", struct, "base", struct, "repo"]),
			[create_message(JSON, "[~s] ~s opened pull request #~b: ~s (" ++ ?BRANCH ++ "~s" ++ ?RESET ++ "..." ++ ?BRANCH ++ "~s" ++ ?RESET ++ ") ~s", [
					{reponame, [struct, "pull_request", struct, "base", struct, "repo"]},
					[struct, "sender", struct, "login"],
					[struct, "pull_request", struct, "number"],
					[struct, "pull_request", struct, "title"],
					[struct, "pull_request", struct, "base", struct, "label"],
					[struct, "pull_request", struct, "head", struct, "label"],
					{url, [struct, "pull_request", struct, "html_url"]}
				])];
		{"reopened", _, error, _} ->
			RepoStruct = traverse_json(JSON, [struct, "pull_request", struct, "base", struct, "repo"]),
			[create_message(JSON, "[~s] ~s reopened pull request #~b: ~s (" ++ ?BRANCH ++ "~s" ++ ?RESET ++ "..." ++ ?BRANCH ++ "~s" ++ ?RESET ++ ") ~s", [
					{reponame, [struct, "pull_request", struct, "base", struct, "repo"]},
					[struct, "sender", struct, "login"],
					[struct, "pull_request", struct, "number"],
					[struct, "pull_request", struct, "title"],
					[struct, "pull_request", struct, "base", struct, "label"],
					[struct, "pull_request", struct, "head", struct, "label"],
					{url, [struct, "pull_request", struct, "html_url"]}
				])];
		{"closed", _, error, _} ->
			RepoStruct = traverse_json(JSON, [struct, "pull_request", struct, "base", struct, "repo"]),
			[create_message(JSON, "[~s] ~s closed pull request #~b: ~s (" ++ ?BRANCH ++ "~s" ++ ?RESET ++ "..." ++ ?BRANCH ++ "~s" ++ ?RESET ++ ") ~s", [
					{reponame, [struct, "pull_request", struct, "base", struct, "repo"]},
					[struct, "sender", struct, "login"],
					[struct, "pull_request", struct, "number"],
					[struct, "pull_request", struct, "title"],
					[struct, "pull_request", struct, "base", struct, "label"],
					[struct, "pull_request", struct, "head", struct, "label"],
					{url, [struct, "pull_request", struct, "html_url"]}
				])];
		{"opened", error, _, _} ->
			RepoStruct = traverse_json(JSON, [struct, "repository"]),
			[create_message(JSON, "[~s] ~s opened issue #~b: ~s ~s", [
					{reponame, [struct, "repository"]},
					[struct, "sender", struct, "login"],
					[struct, "issue", struct, "number"],
					[struct, "issue", struct, "title"],
					{url, [struct, "issue", struct, "html_url"]}
				])];
		{"reopened", error, _, _} ->
			RepoStruct = traverse_json(JSON, [struct, "repository"]),
			[create_message(JSON, "[~s] ~s reopened issue #~b: ~s ~s", [
					{reponame, [struct, "repository"]},
					[struct, "sender", struct, "login"],
					[struct, "issue", struct, "number"],
					[struct, "issue", struct, "title"],
					{url, [struct, "issue", struct, "html_url"]}
				])];
		{"closed", error, _, _} ->
			RepoStruct = traverse_json(JSON, [struct, "repository"]),
			[create_message(JSON, "[~s] ~s closed issue #~b: ~s ~s", [
					{reponame, [struct, "repository"]},
					[struct, "sender", struct, "login"],
					[struct, "issue", struct, "number"],
					[struct, "issue", struct, "title"],
					{url, [struct, "issue", struct, "html_url"]}
				])];
		{error, _, _, X} when X /= error ->
			RepoStruct = traverse_json(JSON, [struct, "repository"]),
			case traverse_json(JSON, [struct, "state"]) of
				"success" ->
					logging:log(info, ?MODULE, "Travis SUCCESS: ~s", [traverse_json(JSON, [struct, "commit", struct, "sha"])]),
					ok;
				"pending" ->
					logging:log(info, ?MODULE, "Travis PENDING: ~s", [traverse_json(JSON, [struct, "commit", struct, "sha"])]),
					ok;
				Status ->
					logging:log(info, ?MODULE, "Travis ~s: ~s", [string:to_upper(Status), traverse_json(JSON, [struct, "commit", struct, "sha"])]),
					case case traverse_json(JSON, [struct, "target_url"]) of
						URL when is_list(URL) ->
							os:putenv("url", URL),
							case catch mochijson:decode(util:safe_os_cmd("curl -s $(echo $url | sed 's#travis-ci.org#api.travis-ci.org/repos#')")) of
								{'EXIT',T} -> logging:log(error, ?MODULE, "Error in mochijson: ~p", [T]), error;
								T -> {ok, T}
							end;
						_ -> error
					end of
						{ok, Travis} ->
							RepoId = create_message(JSON, "~s", [{reponame, [struct, "repository"]}]),
							[create_message(Travis, "[~s] Commit ~s '~s' has ~s CI: ~s ~s", [
									{RepoId},
									{hash, [struct, "commit"]},
									{trunc_newline, [struct, "message"]},
									{case Status of
										"failure" -> "failed";
										"error" -> "errored"
									end},
									{create_message(JSON, "~s", [ciurl])},
									[struct, "compare_url"]
								])];
						error ->
							[create_message(JSON, "[~s] Commit ~s on ~s has ~s CI: ~s ~s", [
									{reponame, [struct, "repository"]},
									{hash, [struct, "commit", struct, "sha"]},
									cibranch,
									{case Status of
										"failure" -> "failed";
										"error" -> "errored"
									end},
									cidesc,
									ciurl
								])]
					end
			end;
		{error, _, _, _} ->
			RepoStruct = traverse_json(JSON, [struct, "repository"]),
			case lists:map(fun(T) -> traverse_json(JSON, [struct, T]) end, ["created", "deleted", "forced"]) of
				[true, false, _] ->
					[create_message(JSON, "[~s] ~s created ~s at ~s: ~s", [
							{reponame, [struct, "repository"]},
							[struct, "sender", struct, "login"],
							{ref, [struct, "ref"]},
							{hash, [struct, "after"]},
							{url, [struct, "compare"]}
						])];
				[false, true, _] ->
					[create_message(JSON, "[~s] ~s deleted ~s at ~s", [
							{reponame, [struct, "repository"]},
							[struct, "sender", struct, "login"],
							{ref, [struct, "ref"]},
							{hash, [struct, "before"]}
						])];
				[false, false, Force] ->
					Pushed = if Force -> ?RED ++ "force-pushed" ++ ?RESET; true -> "pushed" end,
					PushMsg = create_message(JSON, "[~s] ~s ~s ~b commit~s to ~s (from ~s to ~s): ~s", [
							{reponame, [struct, "repository"]},
							[struct, "sender", struct, "login"],
							{Pushed},
							{length, [struct, "commits", array]},
							{s, [struct, "commits", array]},
							{ref, [struct, "ref"]},
							{hash, [struct, "before"]},
							{hash, [struct, "after"]},
							{url, [struct, "compare"]}
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
		_ when not is_list(Messages) -> false;
		_ -> true
	end of
		true ->
			case traverse_json(RepoStruct, [struct, "owner", struct, "name"]) of
				error -> UU = traverse_json(RepoStruct, [struct, "owner", struct, "login"]);
				UU -> ok
			end,
			NN = traverse_json(RepoStruct, [struct, "name"]),
			{UU, NN, Messages};
		false -> ok
	end.

create_message(JSON, String, FormatJsonPaths) ->
	io_lib:format(String, lists:map(fun
			({T}) -> T;
			({hash,T}) -> ?HASH ++ lists:sublist(traverse_json(JSON, T), 8) ++ ?RESET;
			({length,T}) -> length(traverse_json(JSON, T));
			({s,Str,T}) -> util:s(length(traverse_json(JSON, T)), Str);
			({s,T}) -> util:s(length(traverse_json(JSON, T)));
			({ref,T}) ->
				case traverse_json(JSON, T) of
					"refs/heads/" ++ Branch -> ?BRANCH ++ Branch ++ ?RESET;
					"refs/tags/" ++ Tag -> ?TAG ++ "tag " ++ Tag ++ ?RESET;
					X -> X
				end;
			({url,T}) ->
				case traverse_json(JSON, T) of
					error -> error;
					URL -> ?URL ++ ?UNDERLINE ++ URL ++ ?UNDERLINE ++ ?RESET
				end;
			({trunc_newline,T}) ->
				case traverse_json(JSON, T) of
					error -> error;
					Lst -> lists:takewhile(fun(X) -> X /= 10 end, Lst)
				end;
			({reponame, RepoStruct}) ->
				case traverse_json(JSON, RepoStruct ++ [struct, "fork"]) of
					false -> ?REPO ++ traverse_json(JSON, RepoStruct ++ [struct, "name"]) ++ ?RESET;
					_ ->
						User = case traverse_json(JSON, RepoStruct ++ [struct, "owner", struct, "login"]) of
							error -> traverse_json(JSON, RepoStruct ++ [struct, "owner", struct, "name"]);
							T -> T
						end,
						?USER ++ User ++ ?RESET ++ "/" ++ ?REPO ++ traverse_json(JSON, RepoStruct ++ [struct, "name"]) ++ ?RESET
				end;
			(cibranch) ->
				SHA = traverse_json(JSON, [struct, "commit", struct, "sha"]),
				case lists:filter(fun(T) -> traverse_json(T, [struct,"commit",struct,"sha"]) == SHA end,
						traverse_json(JSON, [struct, "branches", array])) of
					[Branch | _] -> ["branch ", traverse_json(Branch, [struct, "name"])];
					_ -> "an unknown branch"
				end;
			(ciurl) ->
				case traverse_json(JSON, [struct, "target_url"]) of
					T when is_list(T) -> T;
					_ -> ""
				end;
			(cidesc) ->
				case traverse_json(JSON, [struct, "description"]) of
					T when is_list(T) -> T;
					_ -> ""
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

