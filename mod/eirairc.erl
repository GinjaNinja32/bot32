-module(eirairc).
-compile(export_all).

-include("definitions.hrl").

-define(OPTS, [{capture,all_but_first,list}]).

handle_event(msg_ignored, {User, Channel, Tokens}) ->
	case permissions:hasperm(User, eira) of
		true ->
			String = string:join(Tokens, " "),
			case case re:run(String, "\\*\\*\\* (.*) \\*\\*\\*", ?OPTS) of
				{match, [Message]} -> {"CONSOLE", string:tokens(Message, " ")};
				nomatch ->
					case re:run(String, "\x02?<([^>]+)>\x02? (.+)", ?OPTS) of
						{match, [Nick, Message]} ->
							{re:replace(Nick, <<16#feff/utf8>>, <<>>, [global, {return, list}]), string:tokens(Message, " ")};
						nomatch -> false
					end
			end of
				false -> ok;
				{N, T} ->
					TaggedN = lists:flatten([User#user.nick, "-", N]),
					config:offer_value(data, [call, string:to_lower(TaggedN)], [$@|N]),
					U = User#user.username ++ "-user",
					util:insert_whois(TaggedN, #{
							nick => TaggedN,
							user => U,
							host => User#user.host,
							real => TaggedN,
							channels => [Channel],
							server => "none",
							server_tagline => "I don't exist!",
							operator => false,
							cloak => false,
							registered => false,
							ssl => false,
							fingerprint => none
						}),
					self() ! {irc, {msg, {User#user{nick=TaggedN, username=U}, Channel, T}}}
			end;
		false -> ok
	end;
handle_event(_, _) -> ok.
