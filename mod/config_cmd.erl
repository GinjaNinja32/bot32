-module(config_cmd).

-compile(export_all).

get_commands() ->
	[
		{"cset", fun cset/1, host},
		{"cget", fun cget/1, host},
		{"c", fun c/1, host}
	].

c(#{reply:=Reply, ping:=Ping, params:=Params}) ->
	case erl_scan:string(string:join(Params, " ") ++ ".") of
		{ok, TokLst, _} ->
			case erl_parse:parse_exprs(TokLst) of
				{ok, ExprList} ->
					lists:foreach(fun(T) ->
							case T of
								{match,_ , PA = {cons,_,_,_}, VA} ->
									Path = erl_parse:normalise(PA),
									Val = erl_parse:normalise(VA),
									config:set_value(config, Path, Val),
									core ! {irc, {msg, {Reply, [Ping, io_lib:format("Set config:~p to ~p.", [Path, Val])]}}};
								PA = {cons,_,_,_} ->
									Path = erl_parse:normalise(PA),
									core ! {irc, {msg, {Reply, [Ping, io_lib:format("Value of config:~p: ~p", [Path, config:get_value(config, Path)])]}}};
								X ->
									core ! {irc, {msg, {Reply, [Ping, io_lib:format("Unknown abstract expression ~p", [X])]}}}
							end
						end, ExprList);
				{error, {_,_,Desc}} ->
					{err, ["Error: ", Desc]}
			end;
		{error, Info, Location} ->
			{err, io_lib:format("Error: ~p / ~p", [Info, Location])}
	end.

cget(#{reply:=RT, ping:=RP, params:=Path}) ->
	case parse(lists:flatten(string:join(Path, " "))) of
		{ok, TruePath} ->
			{irc, {msg, {RT, [RP, io_lib:format("~p", [config:get_value(config, TruePath)])]}}};
		{err, T} ->
			{irc, {msg, {RT, [RP, T]}}}
	end.

cset(#{reply:=RT, ping:=RP, params:=Params}) ->
	{Path,[_|Value]} = lists:splitwith(fun(T)->T/=":" end, Params),
	case parse(lists:flatten(string:join(Path, " "))) of
		{err, T} -> {irc, {msg, {RT, [RP, T]}}};
		{ok, TruePath} ->
			case parse(lists:flatten(string:join(Value, " "))) of
				{ok, [Val]} ->
					config:set_value(config, TruePath, Val),
					{irc, {msg, {RT, [RP, io_lib:format("Set ~p to ~p.", [TruePath, Val])]}}};
				{ok, _} ->
					{irc, {msg, {RT, [RP, "Provide a single value to set!"]}}};
				{err, T} ->
					{irc, {msg, {RT, [RP, T]}}}
			end
	end.

parse(Lst) ->
	case erl_scan:string(Lst ++ ".") of
		{ok, TokLst, _} ->
			case erl_parse:parse_exprs(TokLst) of
				{ok, ExprList} ->
					{ok, lists:map(fun erl_parse:normalise/1, ExprList)};
				{error, {_,_,Desc}} ->
					{err, ["Error: ", Desc]}
			end;
		{error, Info, Location} ->
			{err, io_lib:format("Error: ~p / ~p", [Info, Location])}
	end.
