-module(permissions).
-compile(export_all).

-include("definitions.hrl").

message_all_rank(Category, Message, Rank) ->
	logging:log(debug, Rank, "~s: ~s", [Category, Message]),
	lists:foreach(fun
			({nickserv,Nick}) -> core ! {irc, {msg, {Nick, [Category, ": ", Message]}}};
			({N,_,_}) -> core ! {irc, {msg, {N, [Category, ": ", Message]}}}
		end, get_with_rank(Rank)).

get_with_rank(Rank) ->
	orddict:fetch_keys(orddict:filter(fun(_,V) -> lists:member(Rank, V) end,
			config:require_value(config, [permissions]))).

rankof_ns(NS) when is_list(NS) -> rankof_ns(list_to_binary(NS));
rankof_ns(NS) ->
	config:get_value(config, [?MODULE, {nickserv, NS}], []).

rankof(Usr) -> rankof(Usr, none).
rankof(#user{nick=N,username=U,host=SrcH}, Channel) ->
	Whois = util:whois(N),
	H = case Whois of
		#{cloak := false} -> SrcH;
		#{cloak := TrueHost} -> TrueHost;
		_ -> SrcH
	end,
	Permissions = config:require_value(config, [permissions]),
	BinChannel = case Channel of
		none -> none;
		_ -> list_to_binary(Channel)
	end,
	orddict:fold(fun
			({Nick, User, Host}, Perms, PermsSoFar) ->
				case    (re:run(N, [$^,util:regex_star(Nick),$$], [{capture,none}, caseless]) == match)
				andalso (re:run(U, [$^,util:regex_star(User),$$], [{capture,none}]) == match)
				andalso (re:run(H, [$^,util:regex_star(Host),$$], [{capture,none}]) == match) of
					true -> lists:umerge(lists:usort(Perms), PermsSoFar);
					false -> PermsSoFar
				end;
			(Chan, Perms, PermsSoFar) when is_binary(Chan) ->
				if Chan == BinChannel -> lists:umerge(lists:usort(Perms), PermsSoFar);
				   true -> PermsSoFar
				end;
			({nickserv, Nickserv}, Perms, PermsSoFar) ->
				case Whois of
					#{nickserv := Account} ->
						case list_to_binary(Account) == Nickserv of
							true -> lists:umerge(lists:usort(Perms), PermsSoFar);
							false -> PermsSoFar
						end;
					_ -> PermsSoFar
				end
		end, [user], Permissions).

rankof_chan(Channel) ->
	case config:get_value(config, [permissions, list_to_binary(Channel)]) of
		'$none' -> [user];
		List -> List
	end.

hasperm(_, user) -> true;
hasperm(User, Perm) -> lists:member(Perm, rankof(User)).

hasperm(_, _, user) -> true;
hasperm(User, Chan, Perm) -> lists:member(Perm, rankof(User, Chan)).
