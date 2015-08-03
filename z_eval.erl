-module(z_eval).
-compile(export_all).

-include("definitions.hrl").

get_commands() ->
	[
		{"eval", gen_eval(fun eval/1), host},
		{"evalstr", gen_eval_str(fun eval/1), host},
		{"s", fun shl/5, host},
		{"sdrop", fun sdrop/5, host},
		{"sshow", fun sshow/5, host},
		{"math", gen_eval(fun math/1), user},
		{"maths", gen_eval(fun math/1), user}
	].

initialise(T) -> T#state{moduledata=orddict:store(?MODULE, [], T#state.moduledata)}.
deinitialise(T) -> T#state{moduledata=orddict:erase(?MODULE, T#state.moduledata)}.

shl(_, RT, P, Params, State) ->
	PStr = lists:flatten(string:join(Params, " ")),
	String = case lists:last(PStr) of
		$. -> PStr;
		_ -> PStr ++ "."
	end,
	case orddict:find(?MODULE, State#state.moduledata) of
		{ok, Bindings} -> ok;
		_ -> Bindings = []
	end,
	case erl_scan:string(String) of
		{ok, Tokens, _} ->
			case erl_parse:parse_exprs(Tokens) of
				{ok, Forms} ->
					case catch erl_eval:exprs(Forms, Bindings) of
						{value, Value, NewBinds} ->
							core ! {irc, {msg, {RT, [P, io_lib:format("~p", [Value])]}}},
							{setkey, {?MODULE, NewBinds}};
						{'EXIT', {Reason, Stack}} -> {irc, {msg, {RT, [P, format_reasonstack(Reason, Stack)]}}};
						{'EXIT', Term} -> {irc, {msg, {RT, [P, io_lib:format("Code exited with ~p", [Term])]}}};
						Term -> {irc, {msg, {RT, [P, io_lib:format("Code threw ~p", [Term])]}}}
					end;
				T -> {irc, {msg, {RT, [P, io_lib:format("~p", [T])]}}}
			end;
		T -> {irc, {msg, {RT, [P, io_lib:format("~p", [T])]}}}
	end.

sshow(_, RT, P, _, State) ->
	case orddict:find(?MODULE, State#state.moduledata) of
		{ok, V} -> {irc, {msg, {RT, [P, io_lib:format("~p", [V])]}}};
		error -> {irc, {msg, {RT, [P, "No state found."]}}}
	end.

sdrop(_, RT, P, _, _) ->
	core ! {irc, {msg, {RT, [P, "State dropped."]}}},
	{setkey, {?MODULE, []}}.

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
			{cerr, Term} -> {irc, {msg, {ReplyTo, [Ping, Term]}}};
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
					case erl_eval:expr(Form, [{'C',299792458}, {'E',2.718281828459}, {'Pi', math:pi()}], {value, fun lmath/2}, {value, fun nlmath/2}) of
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

lmath(ipow, [B,P]) when is_integer(B) andalso is_integer(P) -> integer_pow(B,P,1);
%lmath(fact, [A]) when is_integer(A) -> factorial(A,1);

lmath(Name, []) -> math:Name();
lmath(Name, [X]) -> math:Name(X);
lmath(Name, [X,Y]) -> math:Name(X,Y);
lmath(Name, _) -> ufunc(Name).

nlmath({erlang,T},[A]) ->
	case T of
		'not' -> not A;
		'bnot' -> bnot A;
		'-' -> - A;
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

integer_pow(_,N,_) when N > 1024 -> throw("Power too large.");
integer_pow(_,0,Y) -> Y;
integer_pow(X,N,Y) -> integer_pow(X*X, N bsr 1, if (N band 1) == 1 -> Y*X; true -> Y end).
