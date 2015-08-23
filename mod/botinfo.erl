-module(botinfo).
-compile(export_all).

-include("definitions.hrl").

get_commands() ->
	[
		{"uptime", fun uptime/5, user},
		{"version", fun version/5, user},
		{"source", fun source/5, user},
		{"github", fun source/5, user}
	].

sectimestamp() -> calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(os:timestamp())).

initialise(T=#state{moduledata=M}) -> T#state{moduledata=orddict:store(botinfo, sectimestamp(), M)}.
deinitialise(T=#state{moduledata=M}) -> T#state{moduledata=orddict:erase(botinfo, M)}.

uptime(_, RT, P, _, #state{moduledata=M}) ->
	StartTime = orddict:fetch(botinfo, M),
	NowTime = sectimestamp(),
	{irc, {msg, {RT, [P, "I have been running for ", common:format_time_difference(NowTime - StartTime)]}}}.

version(_, RT, P, _, _) ->
	% Erlang info
	ErlVer = erlang:system_info(otp_release),

	% System info
	{_, OSname} = os:type(),
	OSver = case os:version() of
		{Maj,Min,Pat} -> [integer_to_list(Maj), $., integer_to_list(Min), $., integer_to_list(Pat)];
		String -> String
	end,

	{irc, {msg, {RT, [P, ?VERSION, " running on Erlang ", ErlVer, " on ", atom_to_list(OSname), $ , OSver, $.]}}}.

source(_, RT, P, _, _) ->
	{irc, {msg, {RT, [P, "http://github.com/GinjaNinja32/bot32"]}}}.
