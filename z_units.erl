-module(z_units).
-compile(export_all).

%-include("definitions.hrl").

get_commands() -> 
	[
		{"units", fun units/5, user}
	].

initialise(T) -> T.
deinitialise(T) -> T.

units(_, RT, P, Params, _) ->
	case case lists:splitwith(fun(T)->T /= "in" andalso T /= "to" andalso T /= "->" end, Params) of
		{Src, []} ->
			os:putenv("units_src", string:join(Src, " ")),
			one;
		{[], [_]} -> "Provide a unit to convert from and a unit to convert to!";
		{[], _} -> "Provide a unit to convert from!";
		{_, [_]} -> "Provide a unit to convert to!";
		{Src, [_|Dst]} ->
			os:putenv("units_src", string:join(Src, " ")),
			os:putenv("units_dst", string:join(Dst, " ")),
			two
	end of
		one -> {irc, {msg, {RT, [P, get_units_reply("units -t \"$units_src\"")]}}};
		two -> {irc, {msg, {RT, [P, get_units_reply("units -t \"$units_src\" \"$units_dst\"")]}}};
		T -> {irc, {msg, {RT, [P, T]}}}
	end.

get_units_reply(Cmd) ->
	Ret = os:cmd(Cmd),
	Toks = string:tokens(Ret, "\n"),
	string:join(Toks, "; ").
