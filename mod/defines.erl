-module(defines).
-compile(export_all).

-include("definitions.hrl").

-define(DefinitionFiles, ["code/setup.dm", "code/modules/hydroponics/_hydro_setup.dm"]).

get_commands() ->
	[
		{"defined", fun defined/4, user}
	].

initialise() ->
	config:set_value(temp, [defines], make_defs()).
deinitialise() ->
	config:delete_value(temp, [defines]).

%

defined(_, RT, P, []) -> {irc, {msg, {RT, [P, "Provide a string to find definitions for!"]}}};
defined(_, RT, P, Params) ->
	{M,DF,D,KP} = config:get_value(temp, [defines], {[],[],[],[]}),
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
	os:cmd("cd /home/bot32/Baystation12; git fetch upstream"),
	Dev = get_defs("dev"),
	DevFreeze = get_defs("dev-freeze"),
	Kunpeng = get_defs("kunpeng"),
	Master = get_defs("master"),
	{Master, DevFreeze, Dev, Kunpeng}.

get_defs(String) ->
	T = extract_defs(os:cmd(
		[
			"cd /home/bot32/Baystation12; ",
			string:join(lists:map(fun(T) -> ["git show origin/",String,":",T] end, ?DefinitionFiles), "; ")
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
