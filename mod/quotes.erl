-module(quotes).
-compile(export_all).

-include("definitions.hrl").

get_aliases() ->
	[
		{"delquote", ["remquote"]},
		{"delquote_c", ["remquote_c"]},
		{"delquote_e", ["remquote_e"]},
		{"delquote_ce", ["remquote_ce"]},
		{"addquote", ["makequote"]}
	].

get_commands() ->
	[
		{"quote", fun quote/1, user},
		{"quotename", fun quotename/1, user},
		{"quoteword", fun quoteword/1, user},
		{"addquote",  fun addquote/1, user},
		{"delquote",    gen_delquote(fun gen_genmatch/2  ), user},
		{"delquote_c",  gen_delquote(fun gen_catmatch/2  ), user},
		{"delquote_e",  gen_delquote(fun gen_exmatch/2   ), user},
		{"delquote_ce", gen_delquote(fun gen_excatmatch/2), user}
	].

%

quote(#{reply:=ReplyTo, ping:=Ping, params:=Params}) ->
	{irc, {msg, {ReplyTo, [Ping, get_quote(string:strip(string:to_lower(string:join(Params, " "))))]}}}.

quotename(#{reply:=ReplyTo, ping:=Ping, params:=Params}) ->
	{irc, {msg, {ReplyTo, [Ping, get_quote_name(string:strip(string:to_lower(string:join(Params, " "))))]}}}.

quoteword(#{reply:=ReplyTo, ping:=Ping, params:=Params}) ->
	{irc, {msg, {ReplyTo, [Ping, get_quote_word(string:strip(string:join(Params, " ")))]}}}.

addquote(#{reply:=ReplyTo, ping:=Ping, params:=[]}) ->
	{irc, {msg, {ReplyTo, [Ping, "Please provide a category and a quote."]}}};
addquote(#{reply:=ReplyTo, ping:=Ping, params:=[_]}) ->
	{irc, {msg, {ReplyTo, [Ping, "Please provide a quote."]}}};
addquote(#{reply:=ReplyTo, ping:=Ping, params:=Params}) ->
	LowerCat = string:to_lower(hd(Params)),
	case lists:all(fun(T) -> ($a =< T andalso T =< $z) orelse ($0 =< T andalso T =< $9) orelse T == $_ end, LowerCat) of
		true ->
			Reply = add_quote(string:to_lower(hd(Params)), string:strip(string:join(tl(Params), " "))),
			{irc, {msg, {ReplyTo, [Ping, Reply]}}};
		false ->
			{irc, {msg, {ReplyTo, [Ping, "Failed to add quote: category must be A-Z, a-z, 0-9 and underscore only. If your quote is something like '<person1> something funny', use 'addquote person1 <person1> something funny'"]}}}
	end.

gen_genmatch(Params, Nick) ->
	Regexed = util:regex_escape(string:to_lower(string:join(Params, " "))),
	fun({C,Q}) -> (C==Nick orelse Nick==admin) andalso re:run(Q, Regexed, [{capture, none}, caseless]) == match end.

gen_exmatch(Params, Nick) ->
	Regexed = util:regex_escape(string:to_lower(string:join(Params, " "))),
	fun({C,Q}) -> (C==Nick orelse Nick==admin) andalso re:run(Q, [$^, Regexed, $$], [{capture, none}, caseless]) == match end.

gen_catmatch(Params, Nick) ->
	Cat = list_to_binary(string:to_lower(hd(Params))),
	General = gen_genmatch(tl(Params), Nick),
	fun(T={C,_}) -> (C==Nick orelse Nick==admin) andalso Cat == C andalso General(T) end.

gen_excatmatch(Params, Nick) ->
	Cat = list_to_binary(string:to_lower(hd(Params))),
	General = gen_exmatch(tl(Params), Nick),
	fun(T={C,_}) -> (C==Nick orelse Nick==admin) andalso Cat == C andalso General(T) end.

gen_delquote(Func) ->
	fun(#{reply:=ReplyTo, ping:=Ping, params:=[]}) -> {irc, {msg, {ReplyTo, [Ping, "Provide a quote to delete!"]}}};
	   (#{reply:=ReplyTo, ping:=Ping, params:=Params, nick:=Nick, ranks:=Ranks}) ->
			UseNick = case lists:member(admin, Ranks) of
				true -> admin;
				false -> list_to_binary(string:to_lower(Nick))
			end,
			case remove_quote(Func(Params, UseNick)) of
				illegal -> {irc, {msg, {ReplyTo, [Ping, "You may not remove that quote."]}}};
				no_match -> {irc, {msg, {ReplyTo, [Ping, "No matching quotes found."]}}};
				{multi_match, Num} -> {irc, {msg, {ReplyTo, [Ping, integer_to_list(Num), " matching quotes found."]}}};
				deleted -> {irc, {msg, {ReplyTo, [Ping, "Quote deleted."]}}}
			end
	end.

%

get_quote([]) ->
	Quotes = config:get_value(data, [?MODULE], []),
	pick_quote(Quotes);
get_quote(String) ->
	Quotes = config:get_value(data, [?MODULE], []),
	Regexed = util:regex_escape(String),
	Matching = lists:filter(fun({Cat, Q}) ->
			re:run(Q, Regexed, [{capture, none}, caseless]) == match orelse Cat == String
		end, Quotes),
	pick_quote(Matching).

remove_quote(MatchFunc) ->
	Quotes = config:get_value(data, [?MODULE], []),
	case lists:filter(MatchFunc, Quotes) of
		[] -> no_match;
		[Quote] ->
			config:set_value(data, [?MODULE], lists:delete(Quote, Quotes)),
			deleted;
		Q -> {multi_match, length(Q)}
	end.

get_quote_name([]) -> "Please supply a username to quote from.";
get_quote_name(String) ->
	Quotes = config:get_value(data, [?MODULE], []),
	Regexed = util:regex_escape(String),
	Matching = lists:filter(fun({Cat, _}) -> re:run(Cat, <<"^", Regexed/binary>>, [{capture, none}, caseless]) == match end, Quotes),
	pick_quote(Matching).

get_quote_word([]) -> "Please supply a word or words to find quotes for.";
get_quote_word(String) ->
	Quotes = config:get_value(data, [?MODULE], []),
	Regexed = util:regex_escape(String),
	Matching = lists:filter(fun({_,Q}) ->
			re:run(Q, [$\\, $b, Regexed, $\\, $b], [{capture, none}, caseless]) == match
		end, Quotes),
	pick_quote(Matching).

pick_quote([]) -> "No matching quotes.";
pick_quote(Q) ->
	{Cat, Quote} = lists:nth(rand:uniform(length(Q)), Q),
	<<"\"", Quote/binary, "\" - ", Cat/binary>>.

add_quote(Category, String) ->
	Quotes = config:get_value(data, [?MODULE], []),
	Regexed = util:regex_escape(String),
	case lists:filter(fun({_, Q}) ->
				re:run(Q, <<"^", Regexed/binary, "$">>, [{capture, none}, caseless]) == match
			end, Quotes) of
		[] ->
			config:set_value(data, [?MODULE], [{list_to_binary(Category), list_to_binary(String)} | Quotes]),
			"Quote added.";
		_ ->
			"Quote is already listed."
	end.
