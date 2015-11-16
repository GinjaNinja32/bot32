-module(unbanme).
-compile(export_all).

-include("definitions.hrl").

get_commands() ->
	[
		{"unban", fun unban/4, user}
	].

origin_mode() -> full.

unban(User, RT, P, _) ->
	case file:consult("ranks.crl") of
		{ok, [Dict]} ->
			Who = string:to_lower(User#user.nick),
			case lists:keyfind(Who, 1, Dict) of
				{_, Rank} ->
					logging:log(info, "unban", "~s used unban; rank is ~s", [User#user.nick, Rank]),
					core ! {raw, ["WHOIS ", User#user.nick]},
					case read_hostname(none) of
						none -> {irc, {msg, {RT, [P, "Could not find your hostname; try again?"]}}};
						Host ->
							logging:log(info, "unban", "hostname for ~s is ~s (source ~s)", [User#user.nick, Host, User#user.host]),
							core ! {irc, {mode, {"#bs12staff", ["-b *!*@", Host]}}},
							core ! {irc, {mode, {"#bs12admin", ["-b *!*@", Host]}}},
							{irc, {msg, {RT, [P, "Unbanned from #bs12staff and #bs12admin"]}}}
					end;
				_ ->
					logging:log(info, "unban", "~s attempted to use unban, but did not have a rank", [User#user.nick]),
					{irc, {msg, {RT, [P, "You are not listed as staff."]}}}
			end;
		error -> {irc, {msg, {RT, [P, "Error: Failed to read staff rank file!"]}}}
	end.

read_hostname(Current) ->
	receive
		{irc, {numeric, {{rpl,whois_user}, [_, _, _, Host | _]}}} -> read_hostname(Host); % true or cloaked hostname
		{irc, {numeric, {{unknown,338}, [_, _, Host, ":has", "cloak"]}}} -> Host; % "has cloak", this is always true hostname if present
		{irc, {numeric, {{unknown,338}, [_, Host, ":has", "cloak"]}}} -> Host; % "has cloak", this is always true hostname if present
		{irc, {numeric, {{rpl,end_of_whois}, _}}} -> Current
	after 1000 ->
		Current
	end.
