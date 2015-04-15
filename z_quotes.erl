-module(z_quotes).
-compile(export_all).

-include("definitions.hrl").

get_commands() ->
	[
		{"quote", fun quote/5, user},
		{"addquote", fun addquote/5, user},
		{"makequote", fun addquote/5, user},
		{"save_quote", fun savequote/5, admin},
		{"load_quote", fun loadquote/5, admin}
	].

get_data(#state{moduledata=M}) ->
	case orddict:find(z_quotes, M) of
		{ok, Value} -> Value;
		error -> []
	end.

set_data(S=#state{moduledata=M}, Data) ->
	S#state{moduledata=orddict:store(z_quotes, Data, M)}.

initialise(T) -> set_data(T, load_quotes()).

deinitialise(T) -> save_quotes(get_data(T)).

%

quote(_, ReplyTo, Ping, Params, State=#state{}) ->
	{irc, {msg, {ReplyTo, [Ping, get_quote(string:strip(string:to_lower(string:join(Params, " "))), get_data(State))]}}}.

addquote(_, ReplyTo, Ping, Params, State=#state{}) ->
	{Reply, Data} = add_quote(string:to_lower(hd(Params)), string:strip(string:join(tl(Params), " ")), get_data(State)),
	self() ! {state, set_data(State, Data)},
	{irc, {msg, {ReplyTo, [Ping, Reply]}}}.

savequote(_, ReplyTo, Ping, _, State=#state{}) ->
	deinitialise(State),
	{irc, {msg, {ReplyTo, [Ping, "Saved quotes."]}}}.

loadquote(_, ReplyTo, Ping, _, State=#state{}) ->
	self() ! {state, set_data(State, load_quotes())},
	{irc, {msg, {ReplyTo, [Ping, "Loaded quotes."]}}}.

%

get_quote([], Quotes) -> pick_quote(Quotes);
get_quote(String, Quotes) ->
	Matching = lists:filter(fun({Cat, Q}) ->
			string:str(string:to_lower(Q), String) /= 0 orelse Cat == String
		end, Quotes),
	pick_quote(Matching).

pick_quote([]) -> "No matching quotes.";
pick_quote(Q) ->
	{Cat, Quote} = lists:nth(random:uniform(length(Q)), Q),
	[Quote, " - ",Cat].

add_quote(Category, String, Quotes) ->
	LowerString = string:to_lower(String),
	case length(lists:filter(fun({_, Q}) ->
				string:to_lower(Q) == LowerString
			end, Quotes)) of
		0 -> {"Quote added.", [{Category, String} | Quotes]};
		_ -> {"Quote is already listed.", Quotes}
	end.

load_quotes() ->
	case file:consult("quotes.crl") of
		{ok, [Term]} ->
			common:debug("QUOTE", "Loaded."),
			Term;
		{error, _} ->
			common:debug("QUOTE", "Creating new."),
			[]
	end.

save_quotes(Quotes) ->
	file:write_file("quotes.crl", io_lib:format("~p.~n", [Quotes])),
	common:debug("QUOTE", "Saved.").

