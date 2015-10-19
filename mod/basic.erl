-module(basic).
-compile(export_all).

get_commands() ->
	[
		{"ping", fun ping/4, user},
		{"pong", fun pong/4, user},
		{"8ball", fun eightball/4, user},
		{"rand", fun rand/4, user},
		{"pick", fun pick/4, user},
		{"dance", fun dance/4, user},
		{"rot13", fun rot_thirteen/4, user},
		{"rot", fun rot_n/4, user},
		{"colors", fun colors/4, user},
		{"colours", fun colors/4, user},
		{"coin", fun coin/4, user}
	].

i2l(T, S) when T < 10 -> [S] ++ integer_to_list(T);
i2l(T, _) -> integer_to_list(T).

colors(_, ReplyTo, Ping, _) -> {irc, {msg, {ReplyTo, [Ping,
		lists:map(fun(X) -> [3,i2l(X,$0),i2l(X,$ )] end, lists:seq(0,15)),
		lists:map(fun(X) -> [3,$,,i2l(X,$0),i2l(X,$ )] end, lists:seq(0,15))
	]}}}.

ping(_, ReplyTo, Ping, _) -> {irc, {msg, {ReplyTo, [Ping, "Pong!"]}}}.
pong(_, ReplyTo, Ping, _) -> {irc, {msg, {ReplyTo, [Ping, "Ping!"]}}}.

eightball(_, ReplyTo, Ping, ["add"|Thing]) ->
	{irc, {msg, {ReplyTo, [Ping, util:addeightball(list_to_binary(string:join(Thing, " ")))]}}};
eightball(_, ReplyTo, Ping, _) ->
        {irc, {msg, {ReplyTo, [Ping, util:eightball()]}}}.

rand(_, ReplyTo, Ping, []) -> {irc, {msg, {ReplyTo, [Ping, "Please pass a positive integer."]}}};
rand(_, ReplyTo, Ping, Params) ->
        {Num, _Rest} = string:to_integer(hd(Params)),
        case Num of
                error -> {irc, {msg, {ReplyTo, [Ping, "Unable to parse integer."]}}};
                T when T > 0 -> {irc, {msg, {ReplyTo, [Ping, erlang:integer_to_list(random:uniform(Num))]}}};
                _ -> {irc, {msg, {ReplyTo, [Ping, "Please pass a positive integer."]}}}
        end.

pick(_, ReplyTo, Ping, []) -> {irc, {msg, {ReplyTo, [Ping, "I need some things to pick from!"]}}};
pick(_, ReplyTo, Ping, Params) -> {irc, {msg, {ReplyTo, [Ping, lists:nth(random:uniform(length(Params)), Params)]}}}.

dance(_, ReplyTo, Ping, _) ->
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

rot_thirteen(_, ReplyTo, Ping, []) -> {irc, {msg, {ReplyTo, [Ping, "Supply a string to rot13!"]}}};
rot_thirteen(_, ReplyTo, Ping, Params) ->
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

rot_n(_, ReplyTo, Ping, []) -> {irc, {msg, {ReplyTo, [Ping, "Supply a number to rotate by and a string to encode!"]}}};
rot_n(_, ReplyTo, Ping, [_]) -> {irc, {msg, {ReplyTo, [Ping, "Supply a string to encode!"]}}};
rot_n(_, ReplyTo, Ping, Params) ->
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

coin(_, RT, P, _) ->
	{irc, {msg, {RT, [P, lists:nth(random:uniform(2), ["Heads!", "Tails!"])]}}}.
