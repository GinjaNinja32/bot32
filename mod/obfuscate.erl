-module(obfuscate).
-compile(export_all).

get_commands() ->
	[
		{"obfuscate", fun obf/1, user}
	].

utf8it(L) -> lists:reverse(utf8it(list_to_binary(L), [])).
utf8it(<<T/utf8, R/binary>>, Out) -> utf8it(R, [T|Out]);
utf8it(<<>>, Out) -> Out;
utf8it(_, _) -> error.

obf(#{reply:=Reply, ping:=Ping, params:=Params, selector:=Selector}) ->
	DefaultObs = [163|"!%?#*"],
	{ObfChars,Fraction} = case Selector of
		[] -> {DefaultObs, 3};
		[T] when $1 =< T andalso T =< $9 -> {DefaultObs, T - $0};
		[T|Rest] when $1 =< T andalso T =< $9 -> {utf8it(Rest), T - $0};
		_ -> {utf8it(Selector), 3}
	end,
	Final = lists:flatmap(fun
			(32) -> [32];
			(46) -> [46];
			(T) ->
				case random:uniform(Fraction) of
					1 ->
						X = lists:nth(random:uniform(length(ObfChars)), ObfChars),
						binary_to_list(<<X/utf8>>);
					_ -> [T]
				end
		end, string:join(Params, " ")),
	{irc, {msg, {Reply, [Ping, Final]}}}.
