-module(z_defines).
-compile(export_all).

-include("definitions.hrl").

-define(DefinitionFiles, ["code/setup.dm", "code/modules/hydroponics/_hydro_setup.dm"]).

get_commands() ->
	[
		{"defined", fun defined/5, user}
	].

initialise(T) -> set_data(T, make_defs()).
deinitialise(T) -> T#state{moduledata=orddict:erase(z_defines, T#state.moduledata)}.

get_data(#state{moduledata=M}) ->
	case orddict:find(z_defines, M) of
		{ok, Value} -> Value;
		error -> make_defs()
	end.

set_data(S=#state{moduledata=M}, Data) ->
	S#state{moduledata=orddict:store(z_defines, Data, M)}.

%

defined(_, RT, P, [], _) -> {irc, {msg, {RT, [P, "Provide a string to find definitions for!"]}}};
defined(_, RT, P, Params, S) ->
	{M,DF,D,KP} = get_data(S),
	{UseDefs, UseParams} = case string:to_lower(hd(Params)) of
		"master" -> {M, tl(Params)};
		"dev-freeze" -> {DF, tl(Params)};
		"devfreeze" -> {DF, tl(Params)};
		"freeze" -> {DF, tl(Params)};
		"dev" -> {D, tl(Params)};
		"kunpeng" -> {KP, tl(Params)};
		"kun-peng" -> {KP, tl(Params)};
		_ -> {M, Params}
	end,
	case UseParams of
		[] -> {irc, {msg, {RT, [P, "Provide a string to find definitions for!"]}}};
		_ ->
			case lists:keyfind(hd(UseParams), 1, UseDefs) of
				{DefKey, DefVal} -> {irc, {msg, {RT, [P, DefKey, " is defined as ", DefVal]}}};
				false -> {irc, {msg, {RT, [P, "No define of ", hd(UseParams), " found."]}}}
			end
	end.

make_defs() ->
	os:cmd("cd /home/nyx/github/baystation12; git fetch upstream"),
	Dev = get_defs("dev"),
	DevFreeze = get_defs("dev-freeze"),
	Kunpeng = get_defs("kunpeng"),
	Master = get_defs("master"),
	{Master, DevFreeze, Dev, Kunpeng}.

get_defs(String) ->
	T = extract_defs(os:cmd(
		[
			"cd /home/nyx/github/baystation12; ",
			string:join(lists:map(fun(T) -> ["git show upstream/",String,":",T] end, ?DefinitionFiles), "; ")
		])),
	logging:log(info, "DEFINES", "Extracted ~b defines from branch '~s'", [length(T), String]),
	T.

extract_defs(String) ->
	Lines = string:tokens(String, "\n"),
	Defines = lists:filter(
		fun
			("#define" ++ _) -> true;
			(_) -> false
		end, Lines),
	Tuples = lists:filtermap(
		fun(T) ->
			case re:run(T, "^#define[\s\t]+([a-zA-Z_0-9]+)[\s\t]+((/?[^/]+)*[^\s\t])([\s\t]*(//.+)?)?$", [{capture, all_but_first, list}]) of
				{match, [DefName, DefVal | _]} -> {true, {DefName, DefVal}};
				nomatch -> false
			end
		end, Defines),
	Tuples.
