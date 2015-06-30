-module(z_dice).
-compile(export_all).

-include("definitions.hrl").

get_commands() ->
	[
		{"dicemode", fun dicemode/5, admin},
		{"dice", fun dice/5, user},
		{"roll", fun dice/5, user},
		{"edice", fun edice/5, user},
		{"eroll", fun edice/5, user},
		{"exroll", fun edice/5, user},
		{"exdice", fun edice/5, user}
	].

initialise(T) -> T.
deinitialise(T) -> T.

get_data(#state{moduledata=M}) ->
        case orddict:find(z_dice, M) of
                {ok, Value} -> Value;
                error ->
			logging:log(error, "DICE", "Data not found, using internal."),
			internal
        end.

set_data(S=#state{moduledata=M}, Data) ->
        S#state{moduledata=orddict:store(z_dice, Data, M)}.


dicemode(_Origin, ReplyTo, Ping, Params, State) ->
	case Params of
		[] ->	{irc, {msg, {ReplyTo, [Ping, "Dice mode is: ", format_dicemode(get_data(State))]}}};
		["internal"] ->
			self() ! {state, set_data(State, internal)},
			{irc, {msg, {ReplyTo, [Ping, "Dice mode is now: ", format_dicemode(internal)]}}};
		["random"] ->
			self() ! {state, set_data(State, random)},
			{irc, {msg, {ReplyTo, [Ping, "Dice mode is now: ", format_dicemode(random)]}}};
		_ ->	{irc, {msg, {ReplyTo, [Ping, "Unknown dice mode: ", string:join(Params, " ")]}}}
	end.

dice(_Origin, ReplyTo, Ping, Params, State) ->
	{irc, {msg, {ReplyTo, [Ping, get_dice(Params, yes, get_data(State))]}}}.
edice(_Origin, ReplyTo, Ping, Params, State) ->
	{irc, {msg, {ReplyTo, [Ping, get_dice(Params, no, get_data(State))]}}}.

format_dicemode(internal) -> "Internal RNG";
format_dicemode(random) -> "random.org";
format_dicemode(Mode) -> io_lib:format("~p", [Mode]).

parse_dice("", Default) -> Default;
parse_dice(Str, _Default) ->
        {S,_} = string:to_integer(Str),
        S.

dice_roll(N, S, M, StrN, StrS, StrM, Collapse, Dicemode) ->
        if
                N == error -> logging:log(error, "ERR", ["to_integer of ",StrN]), "Internal error.";
                S == error -> logging:log(error, "ERR", ["to_integer of ",StrS]), "Internal error.";
                M == error -> logging:log(error, "ERR", ["to_integer of ",StrM]), "Internal error.";
                true -> get_dice(N, S, Collapse, M, Dicemode)
        end.

get_dice([], _, _) -> "Provide a string to roll in the form AdB, or AdB+/-C";
get_dice(Params, Collapse, Dicemode) ->
        R = re:run(hd(Params), "^([0-9]*)d([0-9]*)([\\+-][0-9]+)?", [{capture, all_but_first, list}]),
        case R of
                {match, [StrN, StrS, StrM]} ->
                                N = parse_dice(StrN, 1),
                                S = parse_dice(StrS, 6),
                                case hd(StrM) of
                                        $+ -> M =  parse_dice(tl(StrM), 0);
                                        $- -> M = -parse_dice(tl(StrM), 0)
                                end,
                                dice_roll(N, S, M, StrN, StrS, StrM, Collapse, Dicemode);
                {match, [StrN, StrS]} ->
                                N = parse_dice(StrN, 1),
                                S = parse_dice(StrS, 6),
                                dice_roll(N, S, 0, StrN, StrS, "", Collapse, Dicemode);
                nomatch -> "Unable to parse dice string.";
                _ -> "Error."
        end.

get_dice(N, S, Collapse, Add, Dicemode) ->
        if
                N > 1000000 -> "I won't roll that many dice.";
                S > 1000000 -> ["How do you expect me to roll a d", erlang:integer_to_list(S), "?"];
                S == 0 -> "You think a d0 is easy to roll?";
                N == 0 -> "Why are you using dice commands if you don't want to roll dice?";
                N > 20 andalso Collapse == no -> ["I won't explode that many dice, but your total is ", get_dice(N, S, yes, Add, Dicemode)];
                Dicemode == random andalso N > 100 -> ["Too many for random.org; via internal RNG: ", get_dice(N, S, Collapse, Add, internal)];
                true -> Dice = roll_dice(N, S, Dicemode),
                        Total = lists:foldl(fun(A,B)->A+B end, 0, Dice),
                        show_dice(Dice, Total, Collapse, Add)
        end.

show_dice(Dice, Total, no, 0)            -> io_lib:format("~B : ~lp", [Total, Dice]);
show_dice(Dice, Total, no, M) when M < 0 -> io_lib:format("~B (~B~B) : ~lp", [Total+M, Total, M, Dice]);
show_dice(Dice, Total, no, M)            -> io_lib:format("~B (~B+~B) : ~lp", [Total+M, Total, M, Dice]);
show_dice(_Dice, Total, _, 0)            -> io_lib:format("~B", [Total]);
show_dice(_Dice, Total, _, M) when M < 0 -> io_lib:format("~B (~B~B)", [Total+M, Total, M]);
show_dice(_Dice, Total, _, M)            -> io_lib:format("~B (~B+~B)", [Total+M, Total, M]).

roll_dice(N, S, Dicemode) ->
        case Dicemode of
                random -> roll_dice_random(N, S);
                internal -> roll_dice_internal(N, S, []);
                _ ->
                        logging:log(error, "DICE", "Illegal dice mode '~p', using 'internal' instead!", [Dicemode]),
                        roll_dice_internal(N, S, [])
        end.

roll_dice_internal(0, _, Total) -> Total;
roll_dice_internal(N, S, Total) -> roll_dice_internal(N-1, S, [random:uniform(S) | Total]).

roll_dice_random(N, S) -> random_org:generate(N, 1, S).


