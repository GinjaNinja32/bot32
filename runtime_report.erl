-module(runtime_report).
-export([report_runtimes/3]).

-include("definitions.hrl").

report_runtimes({User,Repo}, Revision, Runtimes) ->
	RepoPath = ["/repos/",User,"/",Repo],
	io:fwrite("~p\n", [Runtimes]),
	lists:foreach(fun(T) ->
				case {orddict:find("id", T), orddict:find("name", T), orddict:find("info", T)} of
					{{ok,ID}, {ok,Name}, {ok,Info}} ->
						case config:get_value(data, [?MODULE, repo, User, Repo, runtime, ID]) of
							'$none' -> report_new_runtime(User, Repo, RepoPath, Revision, ID, Name, Info);
							_ -> update_runtime(User, Repo, RepoPath, Revision, ID, Name, Info)
						end;
					_ -> logging:log(error, ?MODULE, "Invalid runtime specification ~p", [T])
				end
		end, Runtimes).

report_new_runtime(User, Repo, RepoPath, Revision, ID, Title, Info) ->
	Body = lists:flatten([
		"Runtime at ", ID, " on ", Revision, ":\n",
		"```\n",
		Info,"\n",
		"```"
	]),
	case github_request(post,
						[RepoPath, "/issues"],
						json:write({struct,[
							{"body", Body},
							{"labels", {array, ["runtime error ☢"]}},
							{"title", Title}
						]})) of
		error -> logging:log(error, ?MODULE, "Failed to create issue for runtime ~s!", [ID]);
		JSON ->
			IssueID = json:traverse(JSON, [struct,"number"]),
			config:set_value(data, [?MODULE, repo, User, Repo, runtime, ID, lastrev], Revision),
			config:set_value(data, [?MODULE, repo, User, Repo, runtime, ID, info], Info),
			config:set_value(data, [?MODULE, repo, User, Repo, runtime, ID, issueID], IssueID)
	end.

update_runtime(User, Repo, RepoPath, Revision, ID, _Title, Info) ->
	IssueID = config:get_value(data, [?MODULE, repo, User, Repo, runtime, ID, issueID]),
	IssuePath = [RepoPath, "/issues/", integer_to_list(IssueID)],
	case config:get_value(data, [?MODULE, repo, User, Repo, runtime, ID, lastrev]) of
		Revision -> ok;
		_OldRevision ->
			case is_issue_open(RepoPath, IssueID) of
				true ->
					config:set_value(data, [?MODULE, repo, User, Repo, runtime, ID, lastrev], Revision);
				false ->
					config:set_value(data, [?MODULE, repo, User, Repo, runtime, ID, lastrev], Revision),
					config:set_value(data, [?MODULE, repo, User, Repo, runtime, ID, info], Info),

					Reopening = case github_request(post,
					                                IssuePath,
					                                "{\"state\":\"open\"}") of
						error -> "Please reopen";
						_ -> "Reopening"
					end,
					
					Comment = lists:flatten([
						Reopening, " due to reoccurence on newer revision ", Revision, " after being closed:\n",
						"```\n",
						Info,"\n",
						"```"
					]),
					github_request(post,
					               [IssuePath, "/comments"],
					               json:write({struct,[{"body",Comment}]}))
			end
	end.

is_issue_open(RepoPath, ID) ->
	case github_request(get, [RepoPath, "/issues/", integer_to_list(ID)]) of
		error -> error;
		JSON ->
			case json:traverse(JSON, [struct, "state"]) of
				"open" -> true;
				"closed" -> false;
				_ -> error
			end
	end.

github_request(Type, Path) -> github_request(Type, Path, []).
github_request(Type, Path, Body) ->
	Auth = config:require_value(config, [?MODULE, gh_auth]),
	Headers = [
			{"Authorization", lists:flatten(io_lib:format("Basic ~s", [base64:encode(Auth)]))},
			{"User-Agent", ?HTTPVERSION}
	],
	Request = case Body of
		[] -> {lists:flatten(["https://api.github.com",Path]), Headers};
		__ -> {lists:flatten(["https://api.github.com",Path]), Headers, "application/json", lists:flatten(Body)}
	end,
	io:fwrite("request ~p ~p\n", [Type, Request]),
	case httpc:request(Type, Request, [], []) of
		{ok, {{_, Code, _}, _Headers, RecvBody}} when Code >= 200 andalso Code =< 299->
			json:parse(RecvBody);
		Resp ->
			logging:log(error, ?MODULE, "Failed HTTP request to GitHub API: ~p", [Resp]),
			error
	end.
