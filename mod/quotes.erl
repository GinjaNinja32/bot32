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
		{"delquote",    gen_delquote(fun gen_genmatch/1  ), admin},
		{"delquote_c",  gen_delquote(fun gen_catmatch/1  ), admin},
		{"delquote_e",  gen_delquote(fun gen_exmatch/1   ), admin},
		{"delquote_ce", gen_delquote(fun gen_excatmatch/1), admin}
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
	Reply = add_quote(string:to_lower(hd(Params)), string:strip(string:join(tl(Params), " "))),
	{irc, {msg, {ReplyTo, [Ping, Reply]}}}.

gen_genmatch(Params) ->
	Regexed = util:regex_escape(string:to_lower(string:join(Params, " "))),
	fun({_,Q}) -> re:run(Q, Regexed, [{capture, none}, caseless]) == match end.

gen_exmatch(Params) ->
	Regexed = util:regex_escape(string:to_lower(string:join(Params, " "))),
	fun({_,Q}) -> re:run(Q, [$^, Regexed, $$], [{capture, none}, caseless]) == match end.

gen_catmatch(Params) ->
	Cat = list_to_binary(string:to_lower(hd(Params))),
	General = gen_genmatch(tl(Params)),
	fun(T={C,_}) -> Cat == C andalso General(T) end.

gen_excatmatch(Params) ->
	Cat = list_to_binary(string:to_lower(hd(Params))),
	General = gen_exmatch(tl(Params)),
	fun(T={C,_}) -> Cat == C andalso General(T) end.

gen_delquote(Func) ->
	fun(#{reply:=ReplyTo, ping:=Ping, params:=[]}) -> {irc, {msg, {ReplyTo, [Ping, "Provide a quote to delete!"]}}};
	   (#{reply:=ReplyTo, ping:=Ping, params:=Params}) ->
		case remove_quote(Func(Params)) of
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
			re:run(Q, [$\\, $W, Regexed, $\\, $W], [{capture, none}, caseless]) == match
		end, Quotes),
	pick_quote(Matching).

pick_quote([]) -> "No matching quotes.";
pick_quote(Q) ->
	{Cat, Quote} = lists:nth(random:uniform(length(Q)), Q),
	<<"\"", Quote/binary, "\" - ", Cat/binary>>.

add_quote(Category, String) ->
	Quotes = config:get_value(data, [?MODULE], []),
	Regexed = util:regex_escape(String),
	common:debug("quote", Regexed),
	case length(lists:filter(fun({_, Q}) ->
				re:run(Q, <<"^", Regexed/binary, "$">>, [{capture, none}, caseless]) == match
			end, Quotes)) of
		0 ->
			config:set_value(data, [?MODULE], [{list_to_binary(Category), list_to_binary(String)} | Quotes]),
			"Quote added.";
		_ -> "Quote is already listed."
	end.
