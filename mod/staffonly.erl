-module(staffonly).
-compile(export_all).

-include("definitions.hrl").

handle_event(join, {#user{nick=N}, Channel}) ->
	case config:get_value(config, [?MODULE, channels, Channel]) of
		'$none' -> ok;
		T ->
			case config:get_value(config, [server, ranks, T, string:to_lower(N)]) of
				'$none' ->
					core ! {irc, {kick, {Channel, N, "You are not permitted in this channel."}}};
				_ ->
					ok
			end
	end;
handle_event(_, _) -> ok.
