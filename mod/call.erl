-module(call).
-compile(export_all).

get_commands() ->
	[
		{"call", fun call/1, user}
	].

call(#{origin:=User, nick:=OriginNick, reply:=Reply, ping:=Ping, params:=Params}) ->
	case Params of
		[] -> {irc, {msg, {Reply, [Ping, "Supply either a nick or the string 'me', and a name to use!"]}}};
		[_] -> {irc, {msg, {Reply, [Ping, "Supply a name to use!"]}}};
		[RawUsr|Nick] ->
			Usr = case RawUsr of
				"me" -> string:to_lower(OriginNick);
				_ -> string:to_lower(RawUsr)
			end,
			case case {permissions:hasperm(User, admin), string:to_lower(OriginNick)} of
				{true,_} -> ok;
				{_, Usr} -> ok;
				_ -> false
			end of
				false -> {irc, {msg, {Reply, [Ping, "You are not authorised to do that!"]}}};
				ok ->
					Nickname = string:join(Nick, " "),
					config:set_value(data, [?MODULE, Usr], Nickname),
					{irc, {msg, {Reply, [Ping, "Done."]}}}
			end
	end.
