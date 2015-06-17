-module(z_git).
-compile(export_all).

-include("definitions.hrl").

get_commands() ->
	[
		{"search", fun search/5, user},
		{"dump_git", fun dump/5, host},
		{"reload_git", fun reload/5, admin}
	].

initialise(T) -> set_data(T, load_trees()).
deinitialise(T) -> T#state{moduledata=orddict:erase(z_git, T#state.moduledata)}.

get_data(S=#state{moduledata=M}) ->
	case orddict:find(z_git, M) of
		{ok, Value} -> Value;
		error ->
			common:debug("GIT", "Data not found, loading!"),
			Trees = load_trees(),
			self() ! {state, set_data(S, Trees)},
			Trees
	end.

set_data(S=#state{moduledata=M}, Data) ->
	S#state{moduledata=orddict:store(z_git, Data, M)}.

%

search(_, RT, Ping, [], _) -> {irc, {msg, {RT, [Ping, "Please provide a search term."]}}};
search(_Origin, ReplyTo, Ping, Params, State) ->
	{M,DF,D,KP} = get_data(State),
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

dump(_, RT, P, _, S) ->
	{M,DF,D,KP} = get_data(S),
	file:write_file("git-master.crl", io_lib:format("~p.~n", [M])),
	file:write_file("git-devfreeze.crl", io_lib:format("~p.~n", [DF])),
	file:write_file("git-dev.crl", io_lib:format("~p.~n", [D])),
	file:write_file("git-kunpeng.crl", io_lib:format("~p.~n", [KP])),
	{irc, {msg, {RT, [P, "Dumped file trees to text."]}}}.

reload(_, ReplyTo, Ping, _, State) ->
	self() ! {state, set_data(State, load_trees())},
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
	string:tokens(os:cmd(["cd /home/nyx/github/baystation12; git ls-files --with-tree upstream/", Branch]), "\r\n").

search_tree(Tree, String, Branch) ->
	%common:debug("GIT", integer_to_list(length(Tree))),
	%common:debug("GIT", "~p", [Tree]),
	case lists:filter(fun(T) ->
				string:str(string:to_lower(T), String) /= 0
			end, Tree) of
		[] -> "No matches found.";
		[Match] -> ["http://github.com/Baystation12/Baystation12/blob/", Branch, "/", Match];
		Multi ->
			%common:debug("GIT", "~p", [Multi]),
			["Multiple results found: ", join_list_max_len(Multi, "; ", 300)]
	end.

%

join_list_max_len(List, Separator, Length) -> join_list_max_len(List, Separator, Length, []).

join_list_max_len([], _, _, Complete) -> Complete;
join_list_max_len(List, Separator, Length, Complete) when length(hd(List))+length(Separator) > Length -> [Complete, " (", integer_to_list(length(List)), " more)"];
join_list_max_len(List, Separator, Length, []) -> join_list_max_len(tl(List), Separator, Length-length(hd(List)), [hd(List)]);
join_list_max_len(List, Separator, Length, Complete) -> join_list_max_len(tl(List), Separator, Length-length(Separator)-length(hd(List)), [Complete, Separator, hd(List)]).

update() -> os:cmd([
	"cd /home/nyx/github/baystation12;",
	"git fetch upstream"]).
