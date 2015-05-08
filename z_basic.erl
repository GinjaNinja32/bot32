-module(z_basic).
-compile(export_all).

get_commands() ->
	[
		{"ping", fun ping/5, user},
		{"8ball", fun eightball/5, user},
		{"rand", fun rand/5, user},
		{"pick", fun pick/5, user},
		{"dance", fun dance/5, user},
		{"rot13", fun rot_thirteen/5, user},
		{"rot", fun rot_n/5, user}
	].

initialise(T) -> T.
deinitialise(T) -> T.

ping(_, ReplyTo, Ping, _, _) -> {irc, {msg, {ReplyTo, [Ping, "Pong!"]}}}.

eightball(_, ReplyTo, Ping, _, _) ->
        Replies = [
        "It is certain",
        "It is decidedly so",
        "Without a doubt",
        "Yes - definitely",
        "You may rely on it",
        "As I see it, yes",
        "Most likely",
        "Outlook good",
        "Signs point to yes",
        "Yes",
        "Reply hazy, try again",
        "Ask again later",
        "Better not tell you now",
        "Cannot predict now",
        "Concentrate and ask again",
        "Don't count on it",
        "My reply is no",
        "My sources say no",
        "Outlook not so good",
        "Very doubtful"],
        Index = random:uniform(length(Replies)),
        {irc, {msg, {ReplyTo, [Ping, lists:nth(Index, Replies)]}}}.

rand(_, ReplyTo, Ping, [], _) -> {irc, {msg, {ReplyTo, [Ping, "Please pass a positive integer."]}}};
rand(_, ReplyTo, Ping, Params, _) ->
        {Num, _Rest} = string:to_integer(hd(Params)),
        case Num of
                error -> {irc, {msg, {ReplyTo, [Ping, "Unable to parse integer."]}}};
                T when T > 0 -> {irc, {msg, {ReplyTo, [Ping, erlang:integer_to_list(random:uniform(Num))]}}};
                _ -> {irc, {msg, {ReplyTo, [Ping, "Please pass a positive integer."]}}}
        end.

pick(_, ReplyTo, Ping, [], _) -> {irc, {msg, {ReplyTo, [Ping, "I need some things to pick from!"]}}};
pick(_, ReplyTo, Ping, Params, _) -> {irc, {msg, {ReplyTo, [Ping, lists:nth(random:uniform(length(Params)), Params)]}}}.

dance(_, ReplyTo, Ping, _, _) ->
	T = random:uniform(100),
	if
		T < 10 -> 	{multi, [
					{irc, {msg, {ReplyTo, ":D/--<"}}},
					{irc, {msg, {ReplyTo, ":D|--<"}}},
					{irc, {msg, {ReplyTo, ":D\\--<"}}}
				]};
		T < 40 -> {irc, {msg, {ReplyTo, [Ping, "No."]}}};
		true -> {irc, {msg, {ReplyTo, [Ping, "What sort of bot do you think I am?!"]}}}
	end.

rot_thirteen(_, ReplyTo, Ping, [], _) -> {irc, {msg, {ReplyTo, [Ping, "Supply a string to rot13!"]}}};
rot_thirteen(_, ReplyTo, Ping, Params, _) ->
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

rot_n(_, ReplyTo, Ping, [], _) -> {irc, {msg, {ReplyTo, [Ping, "Supply a number to rotate by and a string to encode!"]}}};
rot_n(_, ReplyTo, Ping, [_], _) -> {irc, {msg, {ReplyTo, [Ping, "Supply a string to encode!"]}}};
rot_n(_, ReplyTo, Ping, Params, _) ->
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
