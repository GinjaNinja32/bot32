-module(z_eval).
-compile(export_all).

get_commands() ->
	[
		{"eval", gen_eval(fun eval/1), host},
		{"evalstr", gen_eval_str(fun eval/1), host},
		{"math", gen_eval(fun math/1), user},
		{"maths", gen_eval(fun math/1), user}
	].

initialise(T) -> T.
deinitialise(T) -> T.

gen_eval(Func) ->
	fun(_,ReplyTo,Ping,[    ],_) -> {irc, {msg, {ReplyTo, [Ping, "Provide a string to evaluate!"]}}};
	   (_,ReplyTo,Ping,Params,_) ->
		Raw = lists:flatten(string:join(Params, " ")),
		Str = case lists:last(Raw) of
			$. -> Raw;
			_ -> Raw ++ "."
		end,
		case catch Func(Str) of
			{ok, Value} -> {irc, {msg, {ReplyTo, [Ping, re:replace(io_lib:format("~w", [Value]), "[\r\n]", "")]}}};
			{'EXIT', {Reason, Stack}} -> {irc, {msg, {ReplyTo, [Ping, format_reasonstack(Reason, Stack)]}}};
			{'EXIT', Term} -> {irc, {msg, {ReplyTo, [Ping, io_lib:format("Code exited with ~p", [Term])]}}};
			Term -> {irc, {msg, {ReplyTo, [Ping, io_lib:format("Code threw ~p", [Term])]}}}
		end
	end.

gen_eval_str(Func) ->
	fun(_,ReplyTo,Ping,[    ],_) -> {irc, {msg, {ReplyTo, [Ping, "Provide a string to evaluate!"]}}};
	   (_,ReplyTo,Ping,Params,_) ->
		Raw = lists:flatten(string:join(Params, " ")),
		Str = case lists:last(Raw) of
			$. -> Raw;
			_ -> Raw ++ "."
		end,
		case catch Func(Str) of
			{ok, Value} -> {irc, {msg, {ReplyTo, [Ping, re:replace(io_lib:format("~s", [Value]), "[\r\n]", "")]}}};
			{'EXIT', {Reason, Stack}} -> {irc, {msg, {ReplyTo, [Ping, format_reasonstack(Reason, Stack)]}}};
			{'EXIT', Term} -> {irc, {msg, {ReplyTo, [Ping, io_lib:format("Code exited with ~p", [Term])]}}};
			Term -> {irc, {msg, {ReplyTo, [Ping, io_lib:format("Code threw ~p", [Term])]}}}
		end
	end.

format_reasonstack(Reason, [TopFrame|_]) ->
	io_lib:format("Error: ~p at ~p", [Reason, TopFrame]).

eval(String) ->
	{ok, Tokens, _} = erl_scan:string(String),
	{ok, Forms} = erl_parse:parse_exprs(Tokens),
	{value, Value, _} = erl_eval:exprs(Forms, []),
	{ok, Value}.

math(String) ->
	{ok, Tokens, _} = erl_scan:string(String),
	{ok, [Form]} = erl_parse:parse_exprs(Tokens),
	{value, Value, _} = erl_eval:expr(Form, [{'C',299792458}, {'E',2.718281828459}, {'Pi', math:pi()}], {value, fun lmath/2}, {value, fun nlmath/2}),
	{ok, Value}.

lmath(ipow, [B,P]) when is_integer(B) andalso is_integer(P) -> integer_pow(B,P,1);
lmath(fact, [A]) when is_integer(A) -> factorial(A,1);

lmath(Name, []) -> math:Name();
lmath(Name, [X]) -> math:Name(X);
lmath(Name, [X,Y]) -> math:Name(X,Y);
lmath(Name, _) -> ufunc(Name).

nlmath({erlang,T},[A]) ->
	case T of
		'not' -> not A;
		'bnot' -> bnot A;
		'length' -> length(A);
		_ -> ufunc(erlang,T)
	end;

nlmath({erlang,T},[A,B]) ->
	case T of
		'+' -> A + B;
		'*' -> A * B;
		'/' -> A / B;
		'-' -> A - B;
		'div' -> A div B;
		'rem' -> A rem B;
		'and' -> A and B;
		'or' -> A or B;
		'xor' -> A xor B;
		'band' -> A band B;
		'bor' -> A bor B;
		'bxor' -> A bxor B;
		'==' ->	A == B;
		'/=' -> A /= B;
		'=<' -> A =< B;
		'<' -> A < B;
		'>=' -> A >= B;
		'>' -> A > B;
		'=:=' -> A =:= B;
		'=/=' -> A =/= B;
		'++' -> A ++ B;
		'--' -> A -- B;
		_ -> ufunc(erlang,T)
	end;

nlmath({A,B}, _) -> ufunc(A,B).

ufunc(Mod,Func) -> ufunc(io_lib:format("~s:~s", [Mod, Func])).
ufunc(Func) -> throw(io_lib:format("Unknown function ~s",[Func])).

% Extra math functions for &math

integer_pow(_,0,A) -> A;
integer_pow(B,P,A) -> integer_pow(B,P-1,A*B).

factorial(1,A) -> A;
factorial(K,A) -> factorial(K-1,A*K).
