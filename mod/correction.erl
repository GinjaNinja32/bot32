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
