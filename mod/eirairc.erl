-module(eirairc).
-compile(export_all).

-include("definitions.hrl").

-define(OPTS, [{capture,all_but_first,list}]).

handle_event(msg_ignored, {User, Channel, Tokens}) ->
	case permissions:hasperm(User, eira) of
		true ->
			String = string:join(Tokens, " "),
			case case re:run(String, "\\*\\*\\* (.*) \\*\\*\\*", ?OPTS) of
				{match, [Message]} -> {User#user.nick ++ "-CONSOLE", string:tokens(Message, " ")};
				nomatch ->
					case re:run(String, "<([^>]+)> (.+)", ?OPTS) of
						{match, [Nick, Message]} ->
							{User#user.nick ++ "-" ++ re:replace(Nick, "\\s", "_", [global,{return,list}]), string:tokens(Message, " ")};
						nomatch -> false
					end
			end of
				false -> ok;
				{N, T} ->
					U = User#user.username ++ "-user",
					util:insert_whois(N, #{
							nick => N,
							user => U,
							host => User#user.host,
							real => N,
							channels => [Channel],
							server => "none",
							server_tagline => "I don't exist!",
							operator => false,
							cloak => false,
							registered => false,
							ssl => false,
							fingerprint => none
						}),
					self() ! {irc, {msg, {User#user{nick=N, username=U}, Channel, T}}}
			end;
		false -> ok
	end;
handle_event(_, _) -> ok.
