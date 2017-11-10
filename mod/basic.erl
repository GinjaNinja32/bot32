-module(basic).
-compile(export_all).

get_commands() ->
	[
		{"ping", fun ping/1, user},
		{"pong", fun pong/1, user},
		{"rand", fun rand/1, [integer], user},
		{"pick", fun pick/1, [list], user},
		{"dance", fun dance/1, user},
		{"rot13", fun rot_thirteen/1, [{"string", long}], user},
		{"rot", fun rot_n/1, [integer, {"string", long}], user},
		{"colors", fun colors/1, user},
		{"colours", fun colors/1, user},
		{"coin", fun coin/1, user}
	].

rainbow() ->
	R = fun(Str) ->
			lists:map(fun({Chr,Col})->
					[3,if Col<10->$0;true->[]end,integer_to_list(Col),Chr]
				end,
				lists:zip(Str,
					lists:flatten(
						[lists:duplicate(length(Str) div 13, lists:seq(2,14)),
						lists:seq(2, 1 + (length(Str) rem 13))
						]
					)
				)
			) end.

alt_funcs() -> [fun select_or_string/1].

select_or_string(Tokens) ->
	case collapse_or_string(Tokens, [], []) of
		false -> false;
		[] -> false;
		[_] -> false;
		Options -> util:pick_rand(Options)
	end.

collapse_or_string([], [], _) -> false;
collapse_or_string([], COpt, Options) -> [COpt | Options];
collapse_or_string(["or"|_], [], _) -> false;
collapse_or_string(["or"|L], COpt, Options) -> collapse_or_string(L, [], [COpt | Options]);
collapse_or_string([T|L], [], Options) -> collapse_or_string(L, [T], Options);
collapse_or_string([T|L], COpt, Options) -> collapse_or_string(L, [COpt,32|T], Options).


i2l(T, S) when T < 10 -> [S] ++ integer_to_list(T);
i2l(T, _) -> integer_to_list(T).

colors(#{reply:=ReplyTo, ping:=Ping}) ->
	core ! {irc, {msg, {ReplyTo, [Ping,
		lists:map(fun(X) -> [3,i2l(X,$0),i2l(X,$ ),32] end, lists:seq(0,15))
	]}}},

	core ! {irc, {msg, {ReplyTo, [Ping,
		lists:map(fun(X) -> [3,$,,i2l(X,$0),i2l(X,$ ),32] end, lists:seq(0,15))
	]}}},

	lists:foreach(fun(T) ->
		core ! {irc, {msg, {ReplyTo, [Ping,
			lists:map(fun(X) -> [3,i2l(X+T,$0),i2l(X+T,$ ),32] end, lists:seq(0,11))
		]}}}
		end, [16, 28, 40, 52, 64, 76, 88]).

ping(#{reply:=ReplyTo, ping:=Ping}) -> {irc, {msg, {ReplyTo, [Ping, "Pong!"]}}}.
pong(#{reply:=ReplyTo, ping:=Ping}) -> {irc, {msg, {ReplyTo, [Ping, "Ping!"]}}}.

rand(#{reply:=ReplyTo, ping:=Ping, params:=[Num]}) ->
	case Num > 0 of
		true -> {irc, {msg, {ReplyTo, [Ping, erlang:integer_to_list(random:uniform(Num))]}}};
		false -> {irc, {msg, {ReplyTo, [Ping, "Please pass a positive integer."]}}}
	end.

pick(#{reply:=ReplyTo, ping:=Ping, params:=[List]}) ->
	{irc, {msg, {ReplyTo, [Ping, util:pick_rand(List)]}}}.

dancereply() ->
	[
		[":D/--<", ":D|--<", ":D\\--<"],
		[[ping, "No."]],
		[[ping, "What sort of bot do you think I am?!"]],
		["<(^_^<)", "(>^_^)>", "<(^_^<)"],
		["< (^V^) <", "> (^v^) >", "^ (^v^) v", "v (^v^) ^"]
	].
danceweights(Nick) ->
	H = h(Nick),
	put(dance_rng, {H rem 1000000, ((H div 1000000) rem 1000000), H div 1000000000000}),
	lists:map(fun(_) ->
			{N, State} = random:uniform_s(10, get(dance_rng)),
			put(dance_rng, State),
			N
		end, dancereply()).
danceweighted(Nick) ->
	lists:zip(danceweights(Nick), dancereply()).

h(Nick) -> h(Nick, 0).
h([],T) -> T;
h([H|R],T) -> h(R,T*37+H).

dance(#{nick:=Nick, reply:=ReplyTo, ping:=Ping}) ->
	W = danceweighted(Nick),
	Total = lists:foldl(fun({K,_},A) -> K+A end, 0, W),
	Select = random:uniform(Total),
	case catch lists:foldl(fun
			({K,T},A) when A+K >= Select -> throw(T);
			({K,_},A) -> A+K
		end, 0, W) of
		Reply when is_list(Reply) ->
			lists:foreach(fun
					(T) -> core ! {irc, {msg, {ReplyTo, lists:map(fun(ping)->Ping;(X)->X end, T)}}}
				end, Reply);
		X -> io:fwrite("got ~p?\n", [X])
	end.
%	if
%		T > 80 -> {multi, [
%			          {irc, {msg, {ReplyTo, ":D/--<"}}},
%			          {irc, {msg, {ReplyTo, ":D|--<"}}},
%			          {irc, {msg, {ReplyTo, ":D\\--<"}}}
%		          ]};
%		T > 40 -> {irc, {msg, {ReplyTo, [Ping, "No."]}}};
%		true -> {irc, {msg, {ReplyTo, [Ping, "What sort of bot do you think I am?!"]}}}
%	end.

rot_thirteen(#{reply:=ReplyTo, ping:=Ping, params:=[String]}) ->
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

rot_n(#{reply:=ReplyTo, ping:=Ping, params:=[N, String]}) ->
	Rotated = lists:map(fun(T) ->
		if
			T >= $A andalso T =< $Z -> $A + mod(T - $A + N, 26);
			T >= $a andalso T =< $z -> $a + mod(T - $a + N, 26);
			true -> T
		end end, String),
	{irc, {msg, {ReplyTo, [Ping, Rotated]}}}.

coin(#{reply:=RT, ping:=P}) ->
	{irc, {msg, {RT, [P, lists:nth(random:uniform(2), ["Heads!", "Tails!"])]}}}.
