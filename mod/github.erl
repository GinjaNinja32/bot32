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

decode_content(Content, Signature) ->
	case catch json:parse(Content) of
		{'EXIT', T} -> logging:log(error, "GITHUB", "Error parsing JSON: ~p", [T]);
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
			case util:bin2hex(crypto:hmac(sha, Secret, Content), <<>>) of
				RealSignature -> true;
				X ->
					io:fwrite("hashed to ~p, expected ~p\n", [X, RealSignature]),
					false
			end
	end.

handle_decoded(JSON) ->
	Messages = case {
				json:traverse(JSON, [struct, "action"]),
				json:traverse(JSON, [struct, "pull_request"]),
				json:traverse(JSON, [struct, "issue"]),
				json:traverse(JSON, [struct, "state"])
			} of
		{"opened", _, error, _} ->
			RepoStruct = json:traverse(JSON, [struct, "pull_request", struct, "base", struct, "repo"]),
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
			RepoStruct = json:traverse(JSON, [struct, "pull_request", struct, "base", struct, "repo"]),
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
			RepoStruct = json:traverse(JSON, [struct, "pull_request", struct, "base", struct, "repo"]),
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
			RepoStruct = json:traverse(JSON, [struct, "repository"]),
			[create_message(JSON, "[~s] ~s opened issue #~b: ~s ~s", [
					{reponame, [struct, "repository"]},
					[struct, "sender", struct, "login"],
					[struct, "issue", struct, "number"],
					[struct, "issue", struct, "title"],
					{url, [struct, "issue", struct, "html_url"]}
				])];
		{"reopened", error, _, _} ->
			RepoStruct = json:traverse(JSON, [struct, "repository"]),
			[create_message(JSON, "[~s] ~s reopened issue #~b: ~s ~s", [
					{reponame, [struct, "repository"]},
					[struct, "sender", struct, "login"],
					[struct, "issue", struct, "number"],
					[struct, "issue", struct, "title"],
					{url, [struct, "issue", struct, "html_url"]}
				])];
		{"closed", error, _, _} ->
			RepoStruct = json:traverse(JSON, [struct, "repository"]),
			[create_message(JSON, "[~s] ~s closed issue #~b: ~s ~s", [
					{reponame, [struct, "repository"]},
					[struct, "sender", struct, "login"],
					[struct, "issue", struct, "number"],
					[struct, "issue", struct, "title"],
					{url, [struct, "issue", struct, "html_url"]}
				])];
		{error, _, _, X} when X /= error ->
			RepoStruct = json:traverse(JSON, [struct, "repository"]),
			case json:traverse(JSON, [struct, "state"]) of
				"success" ->
					logging:log(info, ?MODULE, "Travis SUCCESS: ~s", [json:traverse(JSON, [struct, "commit", struct, "sha"])]),
					ok;
				"pending" ->
					logging:log(info, ?MODULE, "Travis PENDING: ~s", [json:traverse(JSON, [struct, "commit", struct, "sha"])]),
					ok;
				Status ->
					logging:log(info, ?MODULE, "Travis ~s: ~s", [string:to_upper(Status), json:traverse(JSON, [struct, "commit", struct, "sha"])]),
					case case json:traverse(JSON, [struct, "target_url"]) of
						URL when is_list(URL) ->
							os:putenv("url", URL),
							case catch json:parse(util:safe_os_cmd("curl -s $(echo $url | sed 's#travis-ci.org#api.travis-ci.org/repos#')")) of
								{'EXIT',T} -> logging:log(error, ?MODULE, "Error parsing JSON: ~p", [T]), error;
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
			RepoStruct = json:traverse(JSON, [struct, "repository"]),
			case lists:map(fun(T) -> json:traverse(JSON, [struct, T]) end, ["created", "deleted", "forced"]) of
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
					CommitList = json:traverse(JSON, [struct, "commits", array]),
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
			case json:traverse(RepoStruct, [struct, "owner", struct, "name"]) of
				error -> UU = json:traverse(RepoStruct, [struct, "owner", struct, "login"]);
				UU -> ok
			end,
			NN = json:traverse(RepoStruct, [struct, "name"]),
			{UU, NN, Messages};
		false -> ok
	end.

create_message(JSON, String, FormatJsonPaths) ->
	io_lib:format(String, lists:map(fun
			({T}) -> T;
			({hash,T}) -> ?HASH ++ lists:sublist(json:traverse(JSON, T), 8) ++ ?RESET;
			({length,T}) -> length(json:traverse(JSON, T));
			({s,Str,T}) -> util:s(length(json:traverse(JSON, T)), Str);
			({s,T}) -> util:s(length(json:traverse(JSON, T)));
			({ref,T}) ->
				case json:traverse(JSON, T) of
					"refs/heads/" ++ Branch -> ?BRANCH ++ Branch ++ ?RESET;
					"refs/tags/" ++ Tag -> ?TAG ++ "tag " ++ Tag ++ ?RESET;
					X -> X
				end;
			({url,T}) ->
				case json:traverse(JSON, T) of
					error -> error;
					URL -> ?URL ++ ?UNDERLINE ++ URL ++ ?UNDERLINE ++ ?RESET
				end;
			({trunc_newline,T}) ->
				case json:traverse(JSON, T) of
					error -> error;
					Lst -> lists:takewhile(fun(X) -> X /= 10 end, Lst)
				end;
			({reponame, RepoStruct}) ->
				case json:traverse(JSON, RepoStruct ++ [struct, "fork"]) of
					false -> ?REPO ++ json:traverse(JSON, RepoStruct ++ [struct, "name"]) ++ ?RESET;
					_ ->
						User = case json:traverse(JSON, RepoStruct ++ [struct, "owner", struct, "login"]) of
							error -> json:traverse(JSON, RepoStruct ++ [struct, "owner", struct, "name"]);
							T -> T

						end,
						?USER ++ User ++ ?RESET ++ "/" ++ ?REPO ++ json:traverse(JSON, RepoStruct ++ [struct, "name"]) ++ ?RESET
				end;
			(cibranch) ->
				SHA = json:traverse(JSON, [struct, "commit", struct, "sha"]),
				case lists:filter(fun(T) -> json:traverse(T, [struct,"commit",struct,"sha"]) == SHA end,
						json:traverse(JSON, [struct, "branches", array])) of
					[Branch | _] -> ["branch ", json:traverse(Branch, [struct, "name"])];
					_ -> "an unknown branch"
				end;
			(ciurl) ->
				case json:traverse(JSON, [struct, "target_url"]) of
					T when is_list(T) -> T;
					_ -> ""
				end;
			(cidesc) ->
				case json:traverse(JSON, [struct, "description"]) of
					T when is_list(T) -> T;
					_ -> ""
				end;
			(T) -> json:traverse(JSON, T)
		end, FormatJsonPaths)).


