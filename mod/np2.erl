-module(np2).
-compile(export_all).

get_commands() ->
	[
		{"np", fun np/1, [{"attacker ships",integer},{"attacker tech",integer},{"defender ships", integer},{"defender tech",integer}], user}
	].

np(#{reply:=Reply, ping:=Ping, params:=[AS,AT,DS,DT]}) ->
	{Winner, Ships} = np_sim(AS,AT,DS,DT),
	{irc, {msg, {Reply, [Ping, io_lib:format("~s wins with ~b ships remaining.", [Winner, Ships])]}}}.

np_sim(AS, AT, DS, DT) ->
	if
		AS - (DT+1) =< 0 -> {"Defender", DS};
		DS - (AT  ) =< 0 -> {"Attacker", AS - (DT+1)};
		true -> np_sim(AS - (DT+1), AT, DS - AT, DT)
	end.
