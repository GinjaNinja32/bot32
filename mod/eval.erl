-module(eval).
-compile(export_all).

-include("definitions.hrl").

get_commands() ->
	[
		{"eval", gen_eval(fun eval/1), eval},
		{"evalstr", gen_eval_str(fun eval/1), eval},
		{"s", fun shl/4, eval},
		{"sdrop", fun sdrop/4, eval},
		{"serase", fun serase/4, eval},
		{"sshow", fun sshow/4, eval},
		{"maths", gen_eval(fun math/1), user},
		{"math", gen_eval(fun math/1), user},
		{"sym", fun sym/4, user},
		{"lsym", fun lsym/4, host}
	].

sym(_, RT, P, Par) ->
	os:putenv("sym", string:join(Par, " ")),
	{irc, {msg, {RT, [P, os:cmd("./sympy_eval.sh 5 \"$sym\"")]}}}.
lsym(_, RT, P, Par) ->
	os:putenv("sym", string:join(Par, " ")),
	{irc, {msg, {RT, [P, os:cmd("./sympy_eval.sh 60 \"$sym\"")]}}}.

shl(_, RT, P, Params) ->
	PStr = lists:flatten(string:join(Params, " ")),
	String = case lists:last(PStr) of
		$. -> PStr;
		_ -> PStr ++ "."
	end,
	Bindings = config:get_value(data, [eval, shell], []),
	case erl_scan:string(String) of
		{ok, Tokens, _} ->
			case erl_parse:parse_exprs(Tokens) of
				{ok, Forms} ->
					case catch erl_eval:exprs(Forms, Bindings) of
						{value, Value, NewBinds} ->
							config:set_value(data, [eval, shell], NewBinds),
							{irc, {msg, {RT, [P, io_lib:format("~p", [Value])]}}};
						{'EXIT', {Reason, Stack}} -> {irc, {msg, {RT, [P, format_reasonstack(Reason, Stack)]}}};
						{'EXIT', Term} -> {irc, {msg, {RT, [P, io_lib:format("Code exited with ~p", [Term])]}}};
						Term -> {irc, {msg, {RT, [P, io_lib:format("Code threw ~p", [Term])]}}}
					end;
				T -> {irc, {msg, {RT, [P, io_lib:format("~p", [T])]}}}
			end;
		T -> {irc, {msg, {RT, [P, io_lib:format("~p", [T])]}}}
	end.

sshow(_, RT, P, _) ->
	case config:get_value(data, [eval, shell]) of
		'$none' -> {irc, {msg, {RT, [P, "No state found."]}}};
		V -> {irc, {msg, {RT, [P, io_lib:format("~p", [V])]}}}
	end.

serase(_, RT, P, A) when length(A) /= 1 -> {irc, {msg, {RT, [P, "Provide a single var name."]}}};
serase(_, RT, P, [Var]) ->
	Msg = case config:get_value(data, [eval, shell]) of
		'$none' -> "No vars found.";
		Vars ->
			Atom = list_to_atom(Var),
			case orddict:find(Atom, Vars) of
				{ok, Val} ->
					config:set_value(data, [eval, shell], orddict:erase(Atom, Vars)),
					io_lib:format("Dropped value ~p.", [Val]);
				error ->
					io_lib:format("No variable ~s found.", [Var])
			end
	end,
	{irc, {msg, {RT, [P, Msg]}}}.

sdrop(_, RT, P, _) ->
	config:set_value(data, [eval, shell], []),
	{irc, {msg, {RT, [P, "State dropped."]}}}.

gen_eval(Func) ->
	fun(_,ReplyTo,Ping,[    ]) -> {irc, {msg, {ReplyTo, [Ping, "Provide a string to evaluate!"]}}};
	   (_,ReplyTo,Ping,Params) ->
		Raw = lists:flatten(string:join(Params, " ")),
		Str = case lists:last(Raw) of
			$. -> Raw;
			_ -> Raw ++ "."
		end,
		case catch Func(Str) of
			{ok, Value} -> {irc, {msg, {ReplyTo, [Ping, re:replace(io_lib:format("~w", [Value]), "[\r\n]", "")]}}};
			{'EXIT', {Reason, Stack}} -> {irc, {msg, {ReplyTo, [Ping, format_reasonstack(Reason, Stack)]}}};
			{'EXIT', Term} -> {irc, {msg, {ReplyTo, [Ping, io_lib:format("Code exited with ~p", [Term])]}}};
			{cerr, Term} -> {irc, {msg, {ReplyTo, [Ping, Term]}}};
			Term -> {irc, {msg, {ReplyTo, [Ping, io_lib:format("Code threw ~p", [Term])]}}}
		end
	end.

gen_eval_str(Func) ->
	fun(_,ReplyTo,Ping,[    ]) -> {irc, {msg, {ReplyTo, [Ping, "Provide a string to evaluate!"]}}};
	   (_,ReplyTo,Ping,Params) ->
		Raw = lists:flatten(string:join(Params, " ")),
		Str = case lists:last(Raw) of
			$. -> Raw;
			_ -> Raw ++ "."
		end,
		case catch Func(Str) of
			{ok, Value} -> {irc, {msg, {ReplyTo, [Ping, re:replace(io_lib:format("~s", [Value]), "[\r\n]", "")]}}};
			{'EXIT', {Reason, Stack}} -> {irc, {msg, {ReplyTo, [Ping, format_reasonstack(Reason, Stack)]}}};
			{'EXIT', Term} -> {irc, {msg, {ReplyTo, [Ping, io_lib:format("Code exited with ~p", [Term])]}}};
			{cerr, Term} -> {irc, {msg, {ReplyTo, [Ping, Term]}}};
			Term -> {irc, {msg, {ReplyTo, [Ping, io_lib:format("Code threw ~p", [Term])]}}}
		end
	end.

format_reasonstack(Reason, [TopFrame|_]) ->
	io_lib:format("Error: ~p at ~p", [Reason, TopFrame]).

eval(String) ->
	case erl_scan:string(String) of
		{ok, Tokens, _} ->
			case erl_parse:parse_exprs(Tokens) of
				{ok, Forms} ->
					case erl_eval:exprs(Forms, []) of
						{value, Value, _} -> {ok, Value};
						X -> throw(X)
					end;
				{error,{_,_,Reason}} -> {cerr, Reason};
				X -> throw(X)
			end;
		{error, {_,_,A}, B} -> {cerr, [A, " before '", B, "'"]};
		X -> throw(X)
	end.


math(String) ->
	case erl_scan:string(String) of
		{ok, Tokens, _} ->
			case erl_parse:parse_exprs(Tokens) of
				{ok, [Form]} ->
					case erl_eval:expr(Form, [{'C',299792458}, {'E',2.718281828459}, {'I', {0,1}}, {'Pi', math:pi()}], {eval, fun lmatheval/3}, {value, fun nlmath/2}) of
						{value, Value, _} -> {ok, Value};
						X -> throw(X)
					end;
				{ok, _} -> {cerr, "Too many statements!"};
				{error,{_,_,Reason}} -> {cerr, Reason};
				X -> throw(X)
			end;
		{error, {_,_,A}, B} -> {cerr, [A, " before '", B, "'"]};
		X -> throw(X)
	end.

lmatheval(Func, Args, Bindings) ->
	RealArgs = lists:map(fun(T) ->
			case erl_eval:expr(T, Bindings, {eval, fun lmatheval/3}, {value, fun nlmath/2}) of
				{value, Value, _} -> Value;
				X -> throw(X)
			end
		end, Args),
	{value, lmath(Func, RealArgs), Bindings}.


lmath(sum, [A]) when is_list(A) -> lists:foldl(fun erlang:'+'/2, 0, A);
lmath(avg, [A]) when is_list(A) -> lists:foldl(fun erlang:'+'/2, 0, A) / length(A);
lmath(ipow, [B,P]) when is_integer(B) andalso is_integer(P) -> integer_pow(B,P,1);
%lmath(fact, [A]) when is_integer(A) -> factorial(A,1);

lmath(real, [Z]) -> complex:real(Z);
lmath(imag, [Z]) -> complex:imag(Z);
lmath(conj, [Z]) -> complex:conj(Z);
lmath(cabs, [Z]) -> complex:abs(Z);
lmath(csqrt, [Z]) -> complex:sqrt(Z);
lmath(cpow, [Z,N]) -> complex:pow(Z,N);
lmath(root, [Z,N]) -> complex:root(Z,N);

lmath(Name, []) -> math:Name();
lmath(Name, [X]) -> math:Name(X);
lmath(Name, [X,Y]) -> math:Name(X,Y);
lmath(Name, _) -> ufunc(Name).


nlmath({erlang,T},[A]) ->
%	common:debug("debug", "erlang:~p(~p, ~p)", [T,A]),
	case T of
		'not' -> not A;
		'bnot' -> bnot A;
		'-' when is_tuple(A) -> complex:sub(0,A);
		'-' -> - A;
		'length' -> length(A);
		_ -> ufunc(erlang,T)
	end;

nlmath({erlang,T},[A,B]) ->
%	common:debug("debug", "erlang:~p(~p, ~p)", [T,A,B]),
	case T of
		'+' -> complex:add(A,B);
		'*' -> complex:mul(A,B);
		'/' -> complex:divd(A,B);
		'-' -> complex:sub(A,B);
		'div' -> A div B;
		'rem' -> A rem B;
		'and' -> A and B;
		'or' -> A or B;
		'xor' -> A xor B;
		'band' -> A band B;
		'bor' -> A bor B;
		'bxor' -> A bxor B;
		'bsl' -> A bsl B;
		'bsr' -> A bsr B;
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

ufunc(Mod,Func) -> ufunc(lists:flatten(io_lib:format("~s:~s", [Mod, Func]))).
ufunc(Func) -> throw(lists:flatten(io_lib:format("Unknown function ~s",[Func]))).

% Extra math functions for &math

integer_pow(_,N,_) when N > 1024 -> throw("Power too large.");
integer_pow(_,0,Y) -> Y;
integer_pow(X,N,Y) -> integer_pow(X*X, N bsr 1, if (N band 1) == 1 -> Y*X; true -> Y end).
