-module(z_basic).
-compile(export_all).

get_commands() ->
	[
		{"ping", fun ping/5, user},
		{"8ball", fun eightball/5, user},
		{"rand", fun rand/5, user},
		{"pick", fun pick/5, user}
	].

initialise(T) -> T.
deinitialise(_) -> ok.

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
