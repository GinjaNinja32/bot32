-module(basic).
-compile(export_all).

get_commands() ->
	[
		{"ping", fun ping/1, user},
		{"pong", fun pong/1, user},
		{"8ball", fun eightball/1, user},
		{"rand", fun rand/1, user},
		{"pick", fun pick/1, user},
		{"dance", fun dance/1, user},
		{"rot13", fun rot_thirteen/1, user},
		{"rot", fun rot_n/1, user},
		{"colors", fun colors/1, user},
		{"colours", fun colors/1, user},
		{"coin", fun coin/1, user}
	].

alt_funcs() -> [fun alt_eightball/1, fun select_or_string/1].

alt_eightball(Tokens) ->
	case util:lasttail(util:lasttail(Tokens)) of
		$? -> util:eightball();
		_ -> false
	end.

select_or_string(Tokens) ->
	case collapse_or_string(Tokens, [], []) of
		false -> false;
		[] -> false;
		[_] -> false;
		Options -> lists:nth(random:uniform(length(Options)), Options)
	end.

collapse_or_string([], [], _) -> false;
collapse_or_string([], COpt, Options) -> [COpt | Options];
collapse_or_string(["or"|_], [], _) -> false;
collapse_or_string(["or"|L], COpt, Options) -> collapse_or_string(L, [], [COpt | Options]);
collapse_or_string([T|L], [], Options) -> collapse_or_string(L, [T], Options);
collapse_or_string([T|L], COpt, Options) -> collapse_or_string(L, [COpt,32|T], Options).


i2l(T, S) when T < 10 -> [S] ++ integer_to_list(T);
i2l(T, _) -> integer_to_list(T).

colors(#{reply:=ReplyTo, ping:=Ping}) -> {irc, {msg, {ReplyTo, [Ping,
		lists:map(fun(X) -> [3,i2l(X,$0),i2l(X,$ )] end, lists:seq(0,15)),
		lists:map(fun(X) -> [3,$,,i2l(X,$0),i2l(X,$ )] end, lists:seq(0,15))
	]}}}.

ping(#{reply:=ReplyTo, ping:=Ping}) -> {irc, {msg, {ReplyTo, [Ping, "Pong!"]}}}.
pong(#{reply:=ReplyTo, ping:=Ping}) -> {irc, {msg, {ReplyTo, [Ping, "Ping!"]}}}.


eightball(#{reply:=ReplyTo, ping:=Ping, params:=["add"|Thing]}) ->
	{irc, {msg, {ReplyTo, [Ping, util:addeightball(list_to_binary(string:join(Thing, " ")))]}}};
eightball(#{reply:=ReplyTo, ping:=Ping}) ->
        {irc, {msg, {ReplyTo, [Ping, util:eightball()]}}}.

rand(#{reply:=ReplyTo, ping:=Ping, params:=[]}) -> {irc, {msg, {ReplyTo, [Ping, "Please pass a positive integer."]}}};
rand(#{reply:=ReplyTo, ping:=Ping, params:=Params}) ->
        {Num, _Rest} = string:to_integer(hd(Params)),
        case Num of
                error -> {irc, {msg, {ReplyTo, [Ping, "Unable to parse integer."]}}};
                T when T > 0 -> {irc, {msg, {ReplyTo, [Ping, erlang:integer_to_list(random:uniform(Num))]}}};
                _ -> {irc, {msg, {ReplyTo, [Ping, "Please pass a positive integer."]}}}
        end.

pick(#{reply:=ReplyTo, ping:=Ping, params:=[]}) -> {irc, {msg, {ReplyTo, [Ping, "I need some things to pick from!"]}}};
pick(#{reply:=ReplyTo, ping:=Ping, params:=Params}) -> {irc, {msg, {ReplyTo, [Ping, lists:nth(random:uniform(length(Params)), Params)]}}}.

dance(#{reply:=ReplyTo, ping:=Ping}) ->
	T = random:uniform(100),
	if
		T < 20 -> 	{multi, [
					{irc, {msg, {ReplyTo, ":D/--<"}}},
					{irc, {msg, {ReplyTo, ":D|--<"}}},
					{irc, {msg, {ReplyTo, ":D\\--<"}}}
				]};
		T < 60 -> {irc, {msg, {ReplyTo, [Ping, "No."]}}};
		true -> {irc, {msg, {ReplyTo, [Ping, "What sort of bot do you think I am?!"]}}}
	end.

rot_thirteen(#{reply:=ReplyTo, ping:=Ping, params:=[]}) -> {irc, {msg, {ReplyTo, [Ping, "Supply a string to rot13!"]}}};
rot_thirteen(#{reply:=ReplyTo, ping:=Ping, params:=Params}) ->
	String = lists:flatten(string:join(Params, " ")),
	Rotated = lists:map(fun(T) ->
		if
			T >= $A andalso T =< $M -> T+13;
			T >= $N andalso T =< $Z -> T-13;
			T >= $a andalso T =< $m -> T+13;
			T >= $n andalso T =< $z -> T-13;
			true -> T
		end end, String),
	{irc, {msg, {ReplyTo, [Ping, Rotated]}}}.

mod(X,Y) when X > 0 -> X rem Y;
mod(X,Y) when X < 0 -> Y + X rem Y;
mod(0,_) -> 0.

rot_n(#{reply:=ReplyTo, ping:=Ping, params:=[]}) -> {irc, {msg, {ReplyTo, [Ping, "Supply a number to rotate by and a string to encode!"]}}};
rot_n(#{reply:=ReplyTo, ping:=Ping, params:=[_]}) -> {irc, {msg, {ReplyTo, [Ping, "Supply a string to encode!"]}}};
rot_n(#{reply:=ReplyTo, ping:=Ping, params:=Params}) ->
	StrN = hd(Params),
	case catch list_to_integer(StrN) of
		N when is_integer(N) ->
			String = string:join(tl(Params), " "),
			Rotated = lists:map(fun(T) ->
				if
					T >= $A andalso T =< $Z -> $A + mod(T - $A + N, 26);
					T >= $a andalso T =< $z -> $a + mod(T - $a + N, 26);
					true -> T
				end end, String),
			{irc, {msg, {ReplyTo, [Ping, Rotated]}}};
		_ -> {irc, {msg, {ReplyTo, [Ping, "Supply a valid number!"]}}}
	end.

coin(#{reply:=RT, ping:=P}) ->
	{irc, {msg, {RT, [P, lists:nth(random:uniform(2), ["Heads!", "Tails!"])]}}}.
