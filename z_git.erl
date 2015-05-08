-module(z_git).
-compile(export_all).

-include("definitions.hrl").

get_commands() ->
	[
		{"search", fun search/5, user},
		{"dump_git", fun dump/5, admin},
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
	case string:to_lower(hd(Params)) of
		"master" ->
			Branch = master,
			UseParams = tl(Params);
		"dev" ->
			Branch = dev,
			UseParams = tl(Params);
		"dev-freeze" ->
			Branch = dev_freeze,
			UseParams = tl(Params);
		"kunpeng" ->
			Branch = kunpeng,
			UseParams = tl(Params);
		_ ->
			Branch = master,
			UseParams = Params
	end,
	case UseParams of
		[] -> {irc, {msg, {ReplyTo, [Ping, "Please provide a search term."]}}};
		_ ->
			SearchString = string:to_lower(string:join(UseParams, " ")),
			{M,DF,D,KP} = get_data(State),
			{irc, {msg, {ReplyTo, 
				case Branch of
					master -> search_tree(M, SearchString, "master");
					dev_freeze -> search_tree(DF, SearchString, "dev-freeze");
					dev -> search_tree(D, SearchString, "dev");
					kunpeng -> search_tree(KP, SearchString, "kunpeng")
				end
			}}}
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
	master(), Master=load_tree(),
	dev(), Dev=load_tree(),
	dev_freeze(), DevFreeze=load_tree(),
	kunpeng(), Kunpeng=load_tree(),
	{Master, DevFreeze, Dev, Kunpeng}.

load_tree() ->
	string:tokens(os:cmd("/home/nyx/github/list.sh"), "\r\n").

search_tree(Tree, String, Branch) ->
	%common:debug("GIT", integer_to_list(length(Tree))),
	%common:debug("GIT", "~p", [Tree]),
	case lists:filter(fun(T) ->
				string:str(string:to_lower(T), String) /= 0
			end, Tree) of
		[] -> "No matches found.";
		[Match] -> ["http://github.com/Baystation12/Baystation12/blob/", Branch, "/", Match];
		Multi -> [integer_to_list(length(Multi)), " results found."]
	end.

%

master() -> os:cmd("/home/nyx/github/update.sh master").
dev() -> os:cmd("/home/nyx/github/update.sh dev").
dev_freeze() -> os:cmd("/home/nyx/github/update.sh dev-freeze").
kunpeng() -> os:cmd("/home/nyx/github/update.sh kunpeng").

