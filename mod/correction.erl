-module(correction).
-compile(export_all).

-include("definitions.hrl").

handle_event(msg_nocommand, {#user{nick=Nick}, Chan, Tokens}) ->
	Msg = string:join(Tokens, " "),
	case catch parse_regex(Msg) of
		{ok, Find, Replace, Options} ->
			do_regex(Find, Replace, Options, Chan);
		{error, Err} ->
			core ! {irc, {msg, {Chan, [Nick, ": ", Err]}}};
		_ ->
			add_line(Nick, Chan, msg, list_to_binary(Msg))
	end;
handle_event(ctcp, {action, Chan, #user{nick=Nick}, Tokens}) ->
	add_line(Nick, Chan, action, list_to_binary(string:join(Tokens, " " )));
handle_event(_, _) -> ok.

do_regex(Find, Replace, Options, Chan) ->
	catch lists:foldl(fun({N,Type,T},_) ->
			case re:replace(T, Find, Replace, [{return,binary} | Options]) of
				T -> ok;
				NewT ->
					remove_line(N, Chan, Type, T),
					Show = re:replace(T, Find, [2,Replace,2], [{return, list} | Options]),
					add_line(N, Chan, Type, NewT),
					case Type of
						msg ->
							core ! {irc, {msg, {Chan, ["<", N, "> ", Show]}}};
						action ->
							core ! {irc, {msg, {Chan, ["* ", N, " ", Show]}}}
					end,
					throw(end_iteration)
			end
		end, foo, config:get_value(temp, [?MODULE, lines, Chan], [])).

remove_line(Nick, Chan, Type, Line) ->
	config:mod_get_value(temp, [?MODULE, lines, Chan],
		fun
			('$none') -> [];
			(Lst) -> lists:delete({Nick, Type, Line}, Lst)
		end),
	ok.

add_line(Nick, Chan, Type, Line) ->
	MaxLines = config:get_value(config, [?MODULE, max_lines], 4),
	config:mod_get_value(temp, [?MODULE, lines, Chan],
		fun
			('$none') -> [{Nick, Type, Line}];
			(Lst) -> [{Nick, Type, Line} | lists:sublist(Lst, MaxLines - 1)]
		end),
	ok.

parse_regex([$s,$/|Rest]) ->
	case split_slashes(Rest) of
		[F,R] ->
			{ok, F, R, []};
		[F,R,O] ->
			Opts = lists:map(fun
						($g) -> global;
						($i) -> caseless;
						($u) -> unicode;
						(T) -> throw({error, ["Unknown option character ",T]})
					end, O),
			{ok, F, R, Opts};
		_ -> false
	end;
parse_regex(_) -> false.


split_slashes(Str) -> split_slashes(Str, [], []).

split_slashes([$/|Str], Curr, Lst) -> split_slashes(Str, [], [lists:reverse(Curr)|Lst]);
split_slashes([$\\,$/|Str], Curr, Lst) -> split_slashes(Str, [$/|Curr], Lst);
split_slashes([$\\,Chr|Str], Curr, Lst) -> split_slashes(Str, [[$\\,Chr]|Curr], Lst);
split_slashes([Chr|Str], Curr, Lst) -> split_slashes(Str, [Chr|Curr], Lst);
split_slashes([], Curr, Lst) -> lists:reverse([lists:reverse(Curr) | Lst]).

% KEYBOARD LAYOUTS

get_commands() ->
	[
		{"kbd", fun kbdfix/1, [short, short, short], user}
	].

kbdfix(#{reply:=Reply, ping:=Ping, params:=[Src,Dst,Nick]}) ->
	LNick = string:to_lower(Nick),
	case parsekbd(Src) of
		error -> core ! {irc, {msg, {Reply, [Ping, "Invalid keyboard layout ", Src]}}};
		SrcKbd ->

	case parsekbd(Dst) of
		error -> core ! {irc, {msg, {Reply, [Ping, "Invalid keyboard layout ", Dst]}}};
		DstKbd ->

	X = (catch lists:foldl(fun({N, Type, T}, _) ->
			case string:to_lower(N) == LNick of
				true ->
					case Type of
						msg ->
							core ! {irc, {msg, {Reply, [Ping, $<, N, $>, $ , convert(T, SrcKbd, DstKbd)]}}};
						action ->
							core ! {irc, {msg, {Reply, [Ping, $*, $ , N, $ , convert(T, SrcKbd, DstKbd)]}}}
					end,
					throw(foo);
				false -> ok
			end
		end, foo, config:get_value(temp, [?MODULE, lines, Reply], []))),
	io:fwrite("~p\n", [X])
	end end,
	ok.

parsekbd(Name) ->
	LName = string:to_lower(Name),
	lists:foldl(fun({L,Lst}, X) ->
			case lists:member(LName, Lst) of
				true -> L;
				false -> X
			end
		end, error, names()).

names() ->
	[
		{dvorak(), ["dvorak", "dv", "d"]},
		{russian(), ["russian", "ru", "r"]},
		{qwerty(), ["qwerty", "qw", "q"]}
	].

indexof(Element, List) -> indexof(Element, List, 1).

indexof(E, <<E/utf8, _/binary>>, N) -> N;
indexof(E, <<_/utf8, R/binary>>, N) -> indexof(E, R, N+1);
indexof(_, <<>>, _) -> 0.

charat(1, <<A/utf8, _/binary>>) -> A;
charat(T, <<_/utf8, R/binary>>) -> charat(T-1, R).

russian() -> <<"ё1234567890-=йцукенгшщзхъ/фывапролджэячсмитьбю.Ё!\"№;%:?*()_+ЙЦУКЕНГШЩЗХЪ|ФЫВАПРОЛДЖЭЯЧСМИТЬБЮ,">>.
dvorak()  -> <<"`1234567890[]',.pyfgcrl/=\aoeuidhtns-;qjkxbmwvz~!@#$%^&*(){}\"<>PYFGCRL?+|AOEUIDHTNS_:QJKXBMWVZ">>.
qwerty()  -> <<"`1234567890-=qwertyuiop[]\asdfghjkl;'zxcvbnm,./~!@#$%^&*()_+QWERTYUIOP{}|ASDFGHJKL:\"ZXCVBNM<>?">>.

convert(String, From, To) ->
    binary_to_list(deseparate(lists:map(fun(S) ->
            case indexof(S, From) of
                0 -> S;
                T -> charat(T, To)
            end
        end, separate(String)))).

separate(<<>>) -> [];
separate(<<T/utf8, R/binary>>) -> [T | separate(R)].

deseparate([]) -> <<>>;
deseparate([T|R]) ->
    DSR = deseparate(R),
    <<T/utf8, DSR/binary>>.

