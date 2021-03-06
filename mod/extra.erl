-module(extra).
-compile(export_all).

showurl(Channel, Ping, URL, Format) -> spawn(?MODULE, showurl_raw, [Channel, Ping, URL, Format, false]).
showurl(Channel, Ping, URL, Format, NotFound) -> spawn(?MODULE, showurl_raw, [Channel, Ping, URL, Format, NotFound]).

showurl_raw(Channel, Ping, URL, Format, NotFound) ->
	os:putenv("url", URL),
	URLAndTitle = util:safe_os_cmd("./urltitle_canonical.sh \"$url\""),
	{CanonicalURL, Title} = lists:splitwith(fun(T) -> T /= 10 end, URLAndTitle),
	case re:replace(Title, "^\\s*|\\s*$", "", [{return, binary}]) of
		<<>> when NotFound /= false -> core ! {irc, {msg, {Channel, [Ping, NotFound]}}};
		<<>> -> ok;
		Sh ->
			core ! {irc, {msg, {Channel, [Ping, io_lib:format(Format, [CanonicalURL, util:parse_htmlentities(Sh)])]}}}
	end.

pre_command(Command, Args) ->
	case is_russian(utf8(list_to_binary(Command))) of
		false -> {Command, Args};
		true ->
			NewArgs = case Args of
				none -> none;
				_ -> de_russian(Args)
			end,
			NewCmd = de_russian_single(Command),

			{NewCmd, NewArgs}
	end.

do_extras(Tokens, ReplyChannel, ReplyPing) ->
	case lists:dropwhile(fun(X) -> re:run(X, "^https?://.*$", [{capture, none}]) /= match end, Tokens) of
		[] -> ok;
		[URL|_] -> showurl(ReplyChannel, ReplyPing, URL, "~i~s")
	end,
	case re:run(string:join(Tokens, " "), "\\[\\[(?:([^ ][^| ]*)\\|)?([^ ][^\]]+[^ ])\\]\\]", [{capture, all_but_first, binary}, global]) of
		{match, Lst} ->
			spawn(fun() -> 
				lists:foreach(fun([Sel, Page]) ->
					WikiURL = case Sel of
						<<>> -> config:get_value(config, [?MODULE, wiki, ReplyChannel], "https://wiki.baystation12.net/");
						T -> config:get_value(config, [?MODULE, wikiselect, T])
					end,
					case WikiURL of
						'$none' ->
							core ! {irc, {msg, {ReplyChannel, [ReplyPing, io_lib:format("wiki '~s' not found", [Sel])]}}};
						_ ->
							UR = WikiURL ++ re:replace(Page, " ", "_", [{return, list}, global]),
							showurl_raw(ReplyChannel, ReplyPing, UR, "~s - ~s", "Page not found!")
					end end,
					Lst)
			      end);
		_ -> ok
	end,
	do_pr_linking(Tokens, ReplyChannel, ReplyPing),
	do_russian(Tokens, ReplyChannel, ReplyPing).

russian_keymap() ->
	[
		{1025,96},{1040,70},{1041,44},{1042,68},{1043,85},{1044,76},
		{1045,84},{1046,58},{1047,80},{1048,66},{1049,81},{1050,82},
		{1051,75},{1052,86},{1053,89},{1054,74},{1055,71},{1056,72},
		{1057,67},{1058,78},{1059,69},{1060,65},{1061,91},{1062,87},
		{1063,88},{1064,73},{1065,79},{1066,93},{1067,83},{1068,77},
		{1069,64},{1070,62},{1071,90},{1072,102},{1073,44},{1074,100},
		{1075,117},{1076,108},{1077,116},{1078,59},{1079,112},{1080,98},
		{1081,113},{1082,114},{1083,107},{1084,118},{1085,121},{1086,106},
		{1087,103},{1088,104},{1089,99},{1090,110},{1091,101},{1092,97},
		{1093,91},{1094,119},{1095,120},{1096,105},{1097,111},{1098,93},
		{1099,115},{1100,109},{1101,39},{1102,46},{1103,122},{1105,96},

		{1110,83}, {1030,115} % Ukrainian і І
	].

do_russian(Tokens, ReplyChannel, ReplyPing) ->
	String = utf8(list_to_binary(string:join(Tokens, " "))),
	case is_russian(String) of
		true -> core ! {irc, {msg, {ReplyChannel, [ReplyPing, "Did you mean: ", util:fix_utf8(convert_russian(String))]}}};
		false -> ok
	end.

de_russian(Tokens) ->
	lists:map(fun de_russian_single/1, Tokens).

de_russian_single(T) ->
	UTFed = utf8(list_to_binary(T)),
	case is_russian(UTFed) of
		true -> convert_russian(UTFed);
		false -> T
	end.

is_russian([]) -> false;
is_russian(String) ->
	NumRussian = lists:foldl(fun(S,N) -> case orddict:is_key(S, russian_keymap()) of true -> N+1; false -> N end end, 0, String),
	NumRussian >= length(String)/3.

convert_russian(String) ->
	lists:map(fun(S) ->
			case orddict:find(S, russian_keymap()) of
				{ok, V} -> V;
				error -> S
			end
		end, String).

utf8(B) -> lists:reverse(utf8(B,[])).
utf8(<<>>, L) -> L;
utf8(<<A/utf8, B/binary>>, L) -> utf8(B, [A | L]).

do_pr_linking(Tokens, Channel, Ping) ->
	lists:foreach(fun(T) -> do_pr_link_token(T, Channel, Ping) end, Tokens).

do_pr_link_token(Token, Channel, Ping) ->
	{DefU, DefR} = config:get_value(config, [?MODULE, github, Channel], {"Baystation12", "Baystation12"}),
	case case re:run(Token, "^([a-zA-Z0-9_\-]+)?(?:/([a-zA-Z0-9_\-]+))?(?:\\[([1-9][0-9]{0,4})\\]|#([1-9][0-9]{3,4}))(?:$|[^a-zA-Z0-9])", [{capture, all_but_first, list}]) of
		{match, [ U,  R, "",  N]} -> {   U,    R, N};
		{match, [ U,  R,      N]} -> {   U,    R, N};
		nomatch -> false
	end of
		{XUser, XRepo, Num} ->
			case list_to_integer(Num) of
				0 -> ok;
				_ ->
					if XUser == [] -> User = DefU; true -> User = XUser end,
					if XRepo == [] -> Repo = DefR; true -> Repo = XRepo end,
					os:putenv("url2", ["http://github.com/", User, $/, Repo, "/issues/", Num]),
					URLTitle = string:strip(re:replace(util:safe_os_cmd("./urltitle.sh \"$url2\""), "([^·]*·[^·]*) · .*", "\\1", [{return, list}])),
					case re:run(URLTitle, "Issue #[0-9]+$", [{capture, none}]) of
						match -> ShowURL = ["http://github.com/", User, $/, Repo, "/issues/", Num];
						nomatch -> ShowURL = ["http://github.com/", User, $/, Repo, "/pull/", Num]
					end,
					core ! {irc, {msg, {Channel, [Ping, $<, ShowURL, "> - ", util:parse_htmlentities(list_to_binary(URLTitle))]}}}
			end;
		false -> ok
	end.
