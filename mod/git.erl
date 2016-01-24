-module(git).
-compile(export_all).

-include("definitions.hrl").

get_commands() ->
	[
		{"search", fun search/1, user},
		{"reload_git", fun reload/1, admin},
		{"defined", fun defined/1, user}
	].

initialise() ->
	load_trees().
deinitialise() ->
	config:del_value(temp, [?MODULE]).

%

branch_params(Params, Type) ->
	case Params of
		[X] -> {"master", [X]};
		[] -> error;
		_ ->
			case config:get_value(temp, [?MODULE, Type, string:to_lower(hd(Params))]) of
				'$none' -> error;
				_ -> {string:to_lower(hd(Params)), tl(Params)}
			end
	end.

search(#{reply:=Reply, ping:=Ping, params:=Params}) ->
	case branch_params(Params, trees) of
		error -> {irc, {msg, {Reply, [Ping, "Provide a valid branch to search!"]}}};
		{BR, PR} ->
			SearchString = string:to_lower(string:join(PR, " ")),
			{irc, {msg, {Reply, [Ping, search_tree(BR, SearchString)]}}}
	end.

reload(#{reply:=ReplyTo, ping:=Ping}) ->
	load_trees(),
	{irc, {msg, {ReplyTo, [Ping, "Reloaded Git file tree"]}}}.

defined(#{reply:=Reply, ping:=Ping, params:=Params}) ->
	case branch_params(Params, defines) of
		error -> {irc, {msg, {Reply, [Ping, "Provide a valid branch to search!"]}}};
		{BR, PR} ->
			Def = hd(PR),
			case config:get_value(temp, [?MODULE, defines, BR, Def]) of
				'$none' -> {irc, {msg, {Reply, [Ping, "No define of '", Def, "' found."]}}};
				{File, Val} -> {irc, {msg, {Reply, [Ping, "'", Def, "' is defined as ", Val, " in ", File, $.]}}}
			end
	end.


%

load_trees() ->
	update(),
	lists:foreach(fun(T) ->
			config:set_value(temp, [?MODULE, trees, T], load_tree(T)),
			config:set_value(temp, [?MODULE, defines, T], load_defines(T))
		end, ["master", "dev-freeze", "dev", "kunpeng"]).

load_defines(Branch) ->
	Home = config:require_value(config, [?MODULE, location]),
	Remote = config:get_value(config, [?MODULE, remote], "upstream"),
	MatchingLines = string:tokens(util:safe_os_cmd(["cd ", Home, "; git grep '#define' ", Remote, "/", Branch]), "\r\n"),
	lists:keysort(1, lists:filtermap(fun(Line) ->
			% origin/dev:code/setup.dm:#define FOO 2
			case re:run(Line, "[^:]+:([^:]+):#define\\s+(\\S+)\\s+(.*)", [{capture,all_but_first,list}]) of
				{match, [File, Def, Val]} ->
					{true, {Def, {File, Val}}};
				nomatch -> false
			end
		end, MatchingLines)).

load_tree(Branch) ->
	Home = config:require_value(config, [?MODULE, location]),
	Remote = config:get_value(config, [?MODULE, remote], "upstream"),
	string:tokens(util:safe_os_cmd(["cd ",Home,"; git ls-files --with-tree ",Remote,"/", Branch]), "\r\n").

search_tree(Branch, String) ->
	Tree = config:require_value(temp, [?MODULE, trees, Branch]),
	case lists:filter(fun(T) ->
				string:str(string:to_lower(T), String) /= 0
			end, Tree) of
		[] -> "No matches found.";
		[Match] -> ["http://github.com/Baystation12/Baystation12/blob/", Branch, "/", re:replace(Match, " ", "%20", [global, {return, list}])];
		Multi ->
			["Multiple results found: ", join_list_max_len(Multi, "; ", 300)]
	end.

%

join_list_max_len(List, Separator, Length) -> join_list_max_len(List, Separator, Length, []).

join_list_max_len([], _, _, Complete) -> Complete;
join_list_max_len(List, Separator, Length, Complete) when length(hd(List))+length(Separator) > Length -> [Complete, " (", integer_to_list(length(List)), " more)"];
join_list_max_len(List, Separator, Length, []) -> join_list_max_len(tl(List), Separator, Length-length(hd(List)), [hd(List)]);
join_list_max_len(List, Separator, Length, Complete) -> join_list_max_len(tl(List), Separator, Length-length(Separator)-length(hd(List)), [Complete, Separator, hd(List)]).

update() ->
	Home = config:require_value(config, [?MODULE, location]),
	Remote = config:get_value(config, [?MODULE, location], "upstream"),
	os:cmd([
		"cd ",Home,";",
		"git fetch ",Remote,";"
	]).
