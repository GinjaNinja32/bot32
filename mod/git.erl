-module(git).
-compile(export_all).

-include("definitions.hrl").

get_commands() ->
	[
		{"search", fun search/4, user},
		{"dump_git", fun dump/4, host},
		{"reload_git", fun reload/4, admin}
	].

initialise() ->
	config:set_value(temp, [?MODULE], load_trees()).
deinitialise() ->
	config:del_value(temp, [?MODULE]).

%

search(_, RT, Ping, []) -> {irc, {msg, {RT, [Ping, "Please provide a search term."]}}};
search(_Origin, ReplyTo, Ping, Params) ->
	{M,DF,D,KP} = config:require_value(temp, [?MODULE]),
	{Tree,Branch,UseParams} = case string:to_lower(hd(Params)) of
		"master" -> {M, "master", tl(Params)};
		"dev" -> {D, "dev", tl(Params)};
		"dev-freeze" -> {DF, "dev-freeze", tl(Params)};
		"devfreeze" -> {DF, "dev-freeze", tl(Params)};
		"freeze" -> {DF, "dev-freeze", tl(Params)};
		"kunpeng" -> {KP, "kunpeng", tl(Params)};
		"kun-peng" -> {KP, "kunpeng", tl(Params)};
		_ -> {M, "master", Params}
	end,
	case UseParams of
		[] -> {irc, {msg, {ReplyTo, [Ping, "Please provide a search term."]}}};
		_ ->
			SearchString = string:to_lower(string:join(UseParams, " ")),
			{irc, {msg, {ReplyTo, search_tree(Tree, SearchString, Branch)}}}
	end.

dump(_, RT, P, _) ->
	{M,DF,D,KP} = config:require_value(temp, [?MODULE]),
	file:write_file("git-master.crl", io_lib:format("~p.~n", [M])),
	file:write_file("git-devfreeze.crl", io_lib:format("~p.~n", [DF])),
	file:write_file("git-dev.crl", io_lib:format("~p.~n", [D])),
	file:write_file("git-kunpeng.crl", io_lib:format("~p.~n", [KP])),
	{irc, {msg, {RT, [P, "Dumped file trees to text."]}}}.

reload(_, ReplyTo, Ping, _) ->
	config:set_value(temp, [?MODULE], load_trees()),
	{irc, {msg, {ReplyTo, [Ping, "Reloaded Git file tree"]}}}.

%

load_trees() ->
	update(),
	Master=load_tree("master"),
	Dev=load_tree("dev"),
	DevFreeze=load_tree("dev-freeze"),
	Kunpeng=load_tree("kunpeng"),
	{Master, DevFreeze, Dev, Kunpeng}.

load_tree(Branch) ->
	Home = config:require_value(config, [?MODULE, location]),
	Remote = config:get_value(config, [?MODULE, remote], "upstream"),
	string:tokens(os:cmd(["cd ",Home,"; git ls-files --with-tree ",Remote,"/", Branch]), "\r\n").

search_tree(Tree, String, Branch) ->
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
