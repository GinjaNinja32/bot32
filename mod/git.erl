-module(git).
-compile(export_all).

-include("definitions.hrl").

get_commands() ->
	[
		{"search", fun search/1, user},
		{"defined", fun defined/1, user}
	].

initialise() ->
	load_trees().
deinitialise() ->
	config:del_value(temp, [?MODULE]).

%

branch_params(Params, Channel, Type) ->
	case Params of
		[_] ->
			ID = config:get_value(config, [?MODULE, default, list_to_binary(Channel)], <<"bay">>),
			{ID, Params};
		[] -> error;
		_ ->
			case config:get_value(temp, [?MODULE, Type, list_to_binary(string:to_lower(hd(Params)))]) of
				'$none' -> error;
				_ -> {list_to_binary(string:to_lower(hd(Params))), tl(Params)}
			end
	end.

do_extras(Tokens, Reply, Ping) ->
	case re:run(string:join(Tokens, " "), "(?:\\s|^)\\[(?:([^[ ][^| ]*)\\|)?([^]# ]*[^]#0-9 ][^]# ]*)(?:(#[^] ]+))?\\](\\s|$)", [{capture, all_but_first, binary}]) of
		{match, [RawID, File, ExtraParams, _]} when byte_size(File) > 2 ->
			ID = case RawID of
				<<>> -> config:get_value(config, [?MODULE, default, list_to_binary(Reply)], <<"bay">>);
				_ -> RawID
			end,
			case search_tree(ID, File, ExtraParams) of
				"No matches found." -> ok;
				Result ->
					core ! {irc, {msg, {Reply, [Ping, Result]}}}
			end;
		{match, [RawID, File, ExtraParams, _]} ->
			logging:log(info, ?MODULE, "~p ~p ~p", [RawID, File, ExtraParams]);
		_ -> ok
	end,
	ok.

search(#{reply:=Reply, ping:=Ping, params:=Params, selector:=Selector}) ->
	Line = case Selector of
		"" -> "";
		_ -> io_lib:format("#L~s", [Selector])
	end,

	case branch_params(Params, Reply, trees) of
		error -> {irc, {msg, {Reply, [Ping, "Provide a valid branch to search!"]}}};
		{BR, PR} ->
			io:fwrite("~p ~p\n", [BR, PR]),
			SearchString = list_to_binary(string:to_lower(string:join(PR, " "))),
			{irc, {msg, {Reply, [Ping, search_tree(BR, SearchString, Line)]}}}
	end.

defined(#{reply:=Reply, ping:=Ping, params:=Params}) ->
	case branch_params(Params, Reply, defines) of
		error -> {irc, {msg, {Reply, [Ping, "Provide a valid branch to search!"]}}};
		{BR, PR} ->
			Def = hd(PR),
			case config:get_value(temp, [?MODULE, defines, BR, list_to_binary(Def)]) of
				'$none' -> {irc, {msg, {Reply, [Ping, "No define of '", Def, "' found."]}}};
				{File, Val} -> {irc, {msg, {Reply, [Ping, "'", Def, "' is defined as ", Val, " in ", File, $.]}}}
			end
	end.


%

load_trees() ->
	lists:foreach(fun(T) ->
			io:fwrite("~s 1\n", [T]),
			update(T),
			io:fwrite("~s 2\n", [T]),
			config:set_value(temp, [?MODULE, trees, T], load_tree(T)),
			io:fwrite("~s 3\n", [T]),
			config:set_value(temp, [?MODULE, defines, T], load_defines(T)),
			io:fwrite("~s 4\n", [T])
		end, config:get_value(config, [?MODULE, defs], [])).

load_defines(ID) ->
	Home = config:require_value(config, [?MODULE, location, ID]),
	Branch = config:require_value(config, [?MODULE, branch, ID]),
	Remote = config:get_value(config, [?MODULE, remote, ID], <<"origin">>),
	MatchingLines = string:tokens(util:safe_os_cmd(["cd ", binary_to_list(Home), "; git grep '#define' ", binary_to_list(Remote), "/", binary_to_list(Branch)]), "\r\n"),
	lists:keysort(1, lists:filtermap(fun(Line) ->
			% origin/dev:code/setup.dm:#define FOO 2
			case re:run(Line, <<"[^:]+:([^:]+):#define\\s+([a-zA-Z0-9_]+)(\\([^)]+\\))?(?:\\s+(.*))?">>, [{capture,all_but_first,binary}]) of
				{match, [File, Def]} ->
					{true, {Def, {File, <<"<no value>">>}}};
				{match, [File, Def, <<"">>, Val]} ->
					{true, {Def, {File, Val}}};
				{match, [File, Def, Args, Val]} ->
					{true, {Def, {File, <<Def/binary, Args/binary, " ", Val/binary>>}}};
				{match, [File, Def, Args]} ->
					{true, {Def, {File, <<Def/binary, Args/binary, " <no value>">>}}};
				nomatch -> false
			end
		end, MatchingLines)).

load_tree(ID) ->
	Home = config:require_value(config, [?MODULE, location, ID]),
	Remote = config:get_value(config, [?MODULE, remote, ID], <<"origin">>),
	Branch = config:get_value(config, [?MODULE, branch, ID], <<"master">>),
	lists:map(fun list_to_binary/1, string:tokens(util:safe_os_cmd(["cd ",binary_to_list(Home),"; git ls-tree --name-only -r ",binary_to_list(Remote),"/", binary_to_list(Branch)]), "\r\n")).

search_tree(ID, RawString, ExtraParams) ->
	Tree = config:require_value(temp, [?MODULE, trees, ID]),
	GHLoc = config:require_value(config, [?MODULE, github, ID]),
	Branch = config:get_value(config, [?MODULE, branch, ID], <<"master">>),
	String = util:bin_to_lower(RawString),
	case lists:filter(fun(T) ->
				binary:match(util:bin_to_lower(T), String) /= nomatch
			end, Tree) of
		[] -> "No matches found.";
		[Match] -> ["https://github.com/", GHLoc, "/blob/", Branch, "/", re:replace(Match, " ", "%20", [global, {return, list}]), ExtraParams];
		Multi ->
			["Multiple results found: ", join_list_max_len(Multi, "; ", 300)]
	end.

%

join_list_max_len(List, Separator, Length) -> join_list_max_len(List, Separator, Length, []).

join_list_max_len([], _, _, Complete) -> Complete;
join_list_max_len(List, Separator, Length, Complete) when byte_size(hd(List))+length(Separator) > Length -> [Complete, " (", integer_to_list(length(List)), " more)"];
join_list_max_len(List, Separator, Length, []) -> join_list_max_len(tl(List), Separator, Length-byte_size(hd(List)), [hd(List)]);
join_list_max_len(List, Separator, Length, Complete) -> join_list_max_len(tl(List), Separator, Length-length(Separator)-byte_size(hd(List)), [Complete, Separator, hd(List)]).

update(ID) ->
	Home = config:require_value(config, [?MODULE, location, ID]),
	Remote = config:get_value(config, [?MODULE, remote, ID], <<"origin">>),
	os:cmd([
		"cd ",binary_to_list(Home),";",
		"git fetch ",binary_to_list(Remote),";"
	]).
