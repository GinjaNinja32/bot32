-module(correction).
-compile(export_all).

-include("definitions.hrl").

handle_event(msg_nocommand, {#user{nick=Nick}, Chan, Tokens}) ->
	Msg = string:join(Tokens, " "),
	case catch parse_regex(Msg) of
		{ok, Find, Replace, Options} ->
			do_regex(Nick, Find, Replace, Options, Chan);
		{error, Msg} ->
			core ! {irc, {msg, {Chan, [Nick, ": ", Msg]}}};
		_ ->
			add_line(Nick, Chan, Msg)
	end;
handle_event(_, _) -> ok.

do_regex(Nick, Find, Replace, Options, Chan) ->
	catch lists:foldl(fun(T,_) ->
			case re:replace(T, Find, Replace, [{return,list} | Options]) of
				T -> ok;
				NewT ->
					Show = re:replace(T, Find, [2,Replace,2], [{return, list} | Options]),
					add_line(Nick, Chan, NewT),
					core ! {irc, {msg, {Chan, [Nick, ": ", Show]}}},
					throw(end_iteration)
			end
		end, foo, config:get_value(temp, [?MODULE, lines, Chan, string:to_lower(Nick)], [])).

add_line(Nick, Chan, Line) ->
	config:mod_get_value(temp, [?MODULE, lines, Chan, string:to_lower(Nick)],
		fun
			('$none') -> [Line];
			(Lst) -> [Line | lists:sublist(Lst, 2)]
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
