-module(steam).
-compile(export_all).

search(Term) ->
	case lists:all(fun(T) -> lists:member(T, "0123456789") end, Term) of
		true -> Term;
		false ->
			os:putenv("steamsearch", Term),
			case util:safe_os_cmd("./steam.sh search \"$steamsearch\"") of
				"" -> error;
				ID -> lists:droplast(ID) % remove trailing \n
			end
	end.

info(ID) -> info(ID, "gb").
info(ID, CC) ->
	os:putenv("steamid", ID),
	os:putenv("steamcc", CC),
	traverse_json(mochijson:decode(util:safe_os_cmd("./steam.sh info \"$steamid\" \"$steamcc\"")),
			[struct, ID, struct, "data"]).

get_commands() ->
	[
		{"steam", fun steam/1, [long], user}
	].

steam(#{reply:=Reply, ping:=Ping, params:=[Param], selector:=[]}) ->
	case search(Param) of
		error -> {irc, {msg, {Reply, [Ping, "No results for '", Param, "'."]}}};
		ID ->
			JSON_UK = info(ID, "uk"),
			JSON_US = info(ID, "us"),
			JSON_EU = info(ID, "fr"), % "eu" here seems to give USD?
			R = create_message([{eu,JSON_EU}, {uk,JSON_UK}, {us,JSON_US}],
								"[STEAM] ~s (~s / ~s / ~s ~s) (~s) https://store.steampowered.com/app/~b/", [
					{utf8fix, [uk, struct, "name"]},
					{price, [uk, struct, "price_overview"]},
					{price, [us, struct, "price_overview"]},
					{price, [eu, struct, "price_overview"]},
					{discount, [uk, struct, "price_overview"]},
					{platforms, [uk, struct, "platforms"]},
					[uk, struct, "steam_appid"]
				]),
			{irc, {msg, {Reply, [Ping, R]}}}
	end;
steam(#{reply:=Reply, ping:=Ping, params:=[Param], selector:=CC}) ->
	case search(Param) of
		error -> {irc, {msg, {Reply, [Ping, "No results for '", Param, "'."]}}};
		ID ->
			JSON = info(ID, CC),
			R = create_message(JSON, "[STEAM] ~s (~s ~s) (~s) https://store.steampowered.com/app/~b/", [
					{utf8fix, [struct, "name"]},
					{price, [struct, "price_overview"]},
					{discount, [struct, "price_overview"]},
					{platforms, [struct, "platforms"]},
					[struct, "steam_appid"]
				]),
			{irc, {msg, {Reply, [Ping, R]}}}
	end.

create_message(JSON, String, FormatJsonPaths) ->
	io_lib:format(String, lists:map(fun
			({T}) -> T;
			({length,T}) -> length(traverse_json(JSON, T));
			({platforms, T}) ->
				Platforms = traverse_json(JSON, T),
				string:join(lists:filter(fun(X)->X/="" end, [
					case traverse_json(Platforms, [struct, "windows"]) of true -> "Win"; false -> "" end,
					case traverse_json(Platforms, [struct, "mac"    ]) of true -> "Mac"; false -> "" end,
					case traverse_json(Platforms, [struct, "linux"  ]) of true -> "Lin"; false -> "" end
				]), "/");
			({price, T}) ->
				case traverse_json(JSON, T) of
					error -> "Free";
					Price ->
						case traverse_json(Price, [struct, "initial"]) == traverse_json(Price, [struct, "final"]) of
							true ->
								create_message(Price, "~s ~s", [
										{currency, [struct,"initial"]},
										[struct, "currency"]
									]);
							false ->
								create_message(Price, "\x0309~s ~s", [
									%	{currency, [struct,"initial"]},
										{currency, [struct,"final"]},
										[struct, "currency"]
									])
						end
				end;
			({discount, T}) ->
				case traverse_json(JSON, T) of
					error -> "";
					Price ->
						case traverse_json(Price, [struct, "initial"]) == traverse_json(Price, [struct, "final"]) of
							true -> "";
							false ->
								create_message(Price, "(-~b%)\x03", [
										[struct, "discount_percent"]
									])
						end
				end;
			({currency,T}) ->
				Amnt = traverse_json(JSON, T),
				io_lib:format("~b.~2..0b", [Amnt div 100, Amnt rem 100]);
			({utf8fix,T}) -> util:fix_utf8(traverse_json(JSON, T));
			(T) -> traverse_json(JSON, T)
		end, FormatJsonPaths)).

traverse_json(error, _) -> error;
traverse_json(JSON, []) -> JSON;
traverse_json({struct,T}, [struct|Path]) -> traverse_json(T, Path);
traverse_json({array,T}, [array|Path]) -> traverse_json(T, Path);
traverse_json(Dict, [Key|Path]) ->
	traverse_json(case lists:keyfind(Key, 1, Dict) of
			false -> error;
			{_,V} -> V
		end, Path).
