-module(units).
-compile(export_all).

alt_funcs() ->
	[
		fun units_alt/1
	].

get_commands() ->
	[
		{"units", fun units/4, user}
	].

units(_, RT, P, Params) ->
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

units_alt(Params) ->
	case lists:splitwith(fun(T) -> T /= "in" andalso T /= "to" andalso T /= "->" end, Params) of
		{Src, [_|Dst]} ->
			os:putenv("units_src", string:join(Src, " ")),
			os:putenv("units_dst", string:join(Dst, " ")),
			get_units_reply("units -t \"$units_src\" \"$units_dst\"");
		_ -> false
	end.

get_units_reply(Cmd) ->
	Ret = os:cmd(Cmd),
	Toks = string:tokens(Ret, "\n"),
	string:join(Toks, "; ").
