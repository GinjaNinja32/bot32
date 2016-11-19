-module(urban).
-compile(export_all).

-include("colordefs.hrl").
%-include("definitions.hrl").

get_commands() ->
	[
		{"ud", fun ud/1, user}
	].

ud(#{reply:=R, ping:=P, params:=Params}) ->
	case lists:all(fun(T) -> lists:member(T,"0123456789") end, hd(Params)) of
		true when tl(Params) /= [] ->
			N = list_to_integer(hd(Params)),
			Term = string:join(tl(Params), " ");
		_ ->
			N = 1,
			Term = string:join(Params, " ")
	end,

	os:putenv("ud", Term),
	UDReply = os:cmd("wget -qT 10 -O - \"http://api.urbandictionary.com/v0/define?term=$ud\""),
	UDFixed = lists:flatmap(fun
			(T) when T < 128 -> [T];
			(T) -> binary_to_list(<<T/utf8>>)
		end, UDReply),
	JSON = json:parse(UDFixed),
	Reply = case json:traverse(JSON, [struct, "list", array]) of
		[] -> "No results found.";
		ResultArray ->
			Result = lists:nth(N, ResultArray),
			re:replace(create_message(Result, "(~b of ~b) ~s: ~s - ~s - ~s", [
					{N}, {length(ResultArray)},
					[struct, "word"],
					[struct, "definition"],
					[struct, "example"],
					{url, [struct, "permalink"]}
				]), "\n+", "  ", [global])
	end,
	{irc, {msg, {R, [P, Reply]}}}.

create_message(JSON, String, FormatJsonPaths) ->
	io_lib:format(String, lists:map(fun
		({T}) -> T;
		({url,T}) ->
			case json:traverse(JSON, T) of
				error -> error;
				URL -> ?CYAN ++ ?UNDERLINE ++ URL ++ ?UNDERLINE ++ ?RESET
			end;
		(T) -> util:fix_utf8(json:traverse(JSON, T))
	end, FormatJsonPaths)).

