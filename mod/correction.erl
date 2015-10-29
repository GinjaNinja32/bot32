-module(correction).
-compile(export_all).

-include("definitions.hrl").

handle_event(msg, {#user{nick=N}, Chan, Tokens}) ->
	Msg = string:join(Tokens, " "),
	case catch parse_regex(Msg) of
		{ok, Find, Replace, Options} ->
			case config:get_value(temp, [?MODULE, string:to_lower(N)]) of
				'$none' -> ok;
				T ->
					New = re:replace(T, Find, Replace, Options),
					config:set_value(temp, [?MODULE, string:to_lower(N)], New),
					core ! {irc, {msg, {Chan, [N, ": ", New]}}}
			end;
		{error, Msg} -> core ! {irc, {msg, {Chan, [N, ": ", Msg]}}};
		_ -> config:set_value(temp, [?MODULE, string:to_lower(N)], Msg)
	end;
handle_event(_, _) -> ok.


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
