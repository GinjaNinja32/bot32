-module(unbanme).
-compile(export_all).

-include("definitions.hrl").

get_commands() ->
	[
		{"unban", fun unban/5, user}
	].

origin_mode() -> full.

unban(User, RT, P, _, _) ->
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
		{irc, {numeric, {{unknown,338}, [_, _, Host | _]}}} -> Host; % "has cloak", this is always true hostname if present
		{irc, {numeric, {{rpl,end_of_whois}, _}}} -> Current
	after 1000 ->
		Current
	end.
