-module(random_org).
-compile(export_all).

generate(Num, Min, Max) when is_integer(Num) andalso is_integer(Min) andalso is_integer(Max) andalso Max >= Min ->
	NumS = erlang:integer_to_list(Num),
	MinS = erlang:integer_to_list(Min),
	MaxS = erlang:integer_to_list(Max),

	Command = lists:flatten([
			"curl -s ", $",
			"https://www.random.org/integers/?num=", NumS, "&min=", MinS, "&max=", MaxS, "&col=1&base=10&format=plain&rnd=new",
			$"
		]),

	NumString = os:cmd(Command),
	NumStrings = string:tokens(NumString, "\n"),
	Numbers = lists:map(fun erlang:list_to_integer/1, NumStrings),

	Numbers;

generate(_, _, _) -> logging:log(error, "RAND", "Potential injection attempt detected!").
	
