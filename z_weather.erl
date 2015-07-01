-module(z_weather).
-compile(export_all).

-define(WEATHER_STRINGS, [
		{"NAME", "Weather for ~s"},
		{"OBSTIME", "Last updated ~s"},
		{"CONDITION", "Conditions: ~s"},
		{"TEMPERATURE", "Temperature: ~s"},
		{"HUMIDITY", "Humidity: ~s"},
		{"WIND", "Wind: ~s"},
		{"PRESSURE", "Pressure: ~s"}
	]).

get_commands() ->
	[
		{"weather", fun weather/5, user}
	].

initialise(T) -> T.
deinitialise(T) -> T.

weather(_, RT, P, Params, _) -> {irc, {msg, {RT, [P, weather(Params)]}}}.

weather([]) -> "Provide a location to look up weather for.";
weather(Place) ->
	os:putenv("location", string:join(Place, "+")),
	Reply = os:cmd("/home/bot32/weather.sh $location"),
	parse_reply(Reply).

parse_reply([]) -> "Error; does the location exist?";
parse_reply(Text) ->
	Lines = string:tokens(Text, "\n"),
	Dict = lists:foldl(fun(X,D) ->
			[K|V] = string:tokens(X, ":"),
			orddict:store(K, string:join(V, ":"), D)
		end, orddict:new(), Lines),
	case lists:member("NAME", orddict:fetch_keys(Dict)) of
		true -> case orddict:fetch("NAME", Dict) of
				", " -> "Error; no name returned; does the location exist?";
				_ ->
					case orddict:fetch_keys(Dict) of
						["NAME"] -> ["No readings found for ", orddict:fetch("NAME", Dict)];
						_ -> create_reply(Dict)
					end
			end;
		false -> "Error; no name returned; does the location exist?"
	end.

create_reply(Dict) ->
	Str = lists:foldr(fun({Key,Format}, String) ->
			case orddict:find(Key, Dict) of
				error -> String;
				{ok,V} -> [io_lib:format(Format, [string:strip(V)]) | String]
			end end, [], ?WEATHER_STRINGS),
	string:join(Str, "; ").
	
