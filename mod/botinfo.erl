-module(botinfo).
-compile(export_all).

-include("definitions.hrl").

get_commands() ->
	[
		{"uptime", fun uptime/1, user},
		{"version", fun version/1, user},
		{"source", fun source/1, user},
		{"github", fun source/1, user}
	].

handle_event(ctcp, {version, _, #user{nick=Nick}, _}) -> core ! {irc, {ctcp_re, {version, Nick, version_string()}}};
handle_event(_, _) -> ok.

sectimestamp() -> calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(os:timestamp())).

initialise() ->
	config:set_value(temp, [botinfo], sectimestamp()).
deinitialise() ->
	config:del_value(temp, [botinfo]).

uptime(#{reply:=RT, ping:=P}) ->
	config:offer_value(temp, [botinfo], sectimestamp()),
	StartTime = config:require_value(temp, [botinfo]),
	NowTime = sectimestamp(),
	{irc, {msg, {RT, [P, "I have been running for ", common:format_time_difference(NowTime - StartTime)]}}}.

version(#{reply:=RT, ping:=P}) ->
	{irc, {msg, {RT, [P | version_string()]}}}.

source(#{reply:=RT, ping:=P}) ->
	{irc, {msg, {RT, [P, "http://github.com/GinjaNinja32/bot32"]}}}.

version_string() ->
	% Erlang info
	ErlVer = erlang:system_info(otp_release),

	% System info
	{_, OSname} = os:type(),
	OSver = case os:version() of
		{Maj,Min,Pat} -> [integer_to_list(Maj), $., integer_to_list(Min), $., integer_to_list(Pat)];
		String -> String
	end,

	io_lib:format("~s running on Erlang ~s on ~p ~s.", [?VERSION, ErlVer, OSname, OSver]).
