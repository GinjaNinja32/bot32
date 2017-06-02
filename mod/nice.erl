-module(nice).
-compile(export_all).

-include("definitions.hrl").

-define(OPTS, [global, {return, binary}]).

filter(Msg) ->
	M1 = util:bin_to_lower(Msg), % Lowercase
	M2 = re:replace(M1, "[\r\n'\"]+", "", ?OPTS), % Remove junk we don't care about
	M3 = re:replace(M2, "\\((.*?)\\)", "\\1", ?OPTS), % |
	M4 = re:replace(M3, "\\[(.*?)\\]", "\\1", ?OPTS), % | Remove *matched* brackets
	M5 = re:replace(M4, "\\{(.*?)\\}", "\\1", ?OPTS), % |
	M6 = re:replace(M5, "<(.*?)>",     "\\1", ?OPTS), % |
	M7 = re:replace(M6, "https?://\\S*", "", ?OPTS), % Remove URLs
	M8 = re:replace(M7, "\\.[\\.\\s]*\\.", "...", ?OPTS), % Replace strings of ..... and spaces with just three
	re:replace(M8, "\s\s+", " ", ?OPTS). % Replace multiple spaces with just one


do_extras(Tokens, Reply, Ping) ->
	String = filter(list_to_binary(string:join(Tokens, " "))),
	Nick = util:regex_escape(string:to_lower(config:require_value(config, [bot, nick]))),
	case re:run(String, ["\\b(", Nick, "|nti|nt)s?\\b"], [{capture, none}]) of
		match ->
			case re:run(String, "\\b(good ?)?night\\b", [{capture, none}]) of
				match -> core ! {irc, {msg, {Reply, [Ping, "Goodnight!"]}}};
				nomatch ->
					case re:run(String, "\\bmorning\\b", [{capture, none}]) of
						match -> core ! {irc, {msg, {Reply, [Ping, "Morning!"]}}};
						nomatch ->
							case re:run(String, "\\bh(ello|i)\\b", [{capture, none}]) of
								match -> core ! {irc, {msg, {Reply, [Ping, "Hello!"]}}};
								nomatch ->
									case re:run(String, "\\b(good ?)?bye\\b", [{capture, none}]) of
										match -> core ! {irc, {msg, {Reply, [Ping, "Goodbye!"]}}};
										nomatch -> ok
									end
							end
					end
			end;
		nomatch -> ok
	end.
