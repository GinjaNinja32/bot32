-module(eirairc).
-compile(export_all).

-include("definitions.hrl").

handle_event(msg_nocommand, {User, Channel, Tokens}) ->
	case permissions:hasperm(User, eira) of
		true ->
			case case hd(Tokens) of
				"***" ->
					{User#user.nick ++ "-CONSOLE", tl(util:droplast(Tokens))};
				[$< | Nick] ->
					{User#user.nick ++ "-" ++ util:droplast(Nick), tl(Tokens)};
				_ ->
					io:fwrite("user ~p has +eira but their first token ~p was not formatted correctly!\n", [User, hd(Tokens)]),
					false
			end of
				false -> ok;
				{N, T} ->
					self() ! {irc, {msg, {User#user{nick=N, username=User#user.username ++ "-user"}, Channel, T}}}
			end;
		false -> ok
	end;
handle_event(_, _) -> ok.
