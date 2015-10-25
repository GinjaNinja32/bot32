-module(config_cmd).

-compile(export_all).

get_commands() ->
	[
		{"config", fun config/4, host}
	].

config(_, RT, RP, ["get"|Path]) ->
	TruePath = lists:map(fun val/1, Path),
	{irc, {msg, {RT, [RP, pval(config:get_value(config, TruePath))]}}};
config(_, RT, RP, ["set"|PathValue]) ->
	{Path,[_|Value]} = lists:splitwith(fun(T)->T/=":" end, PathValue),
	TruePath = lists:map(fun val/1, Path),
	TrueValue = string:join(Value, " "),
	config:set_value(config, TruePath, TrueValue),
	{irc, {msg, {RT, [RP, "Done."]}}};
config(_, RT, RP, _) ->
	{irc, {msg, {RT, [RP, "Use 'config get [key]' or 'config set [key] : [value]'."]}}}.

val([$'|Atom]) -> list_to_atom(Atom);
val(T) -> T.

pval(T) ->
	io_lib:format("~p", [T]).
