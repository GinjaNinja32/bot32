-module(eval).
-compile(export_all).

-include("definitions.hrl").

get_commands() ->
	[
		{"eecho", fun eecho/1, eval},
		{"eval", gen_eval(fun eval/1), eval},
		{"evalstr", gen_eval_str(fun eval/1), eval},
		{"bash", fun bash/1, [long], eval},
		{"bashl", fun bashl/1, [long], eval},
		{"bashld", fun bashld/1, [long], eval},
		{"py", fun py/1, [long], eval},
		{"pyl", fun pyl/1, [long], eval},
		{"s", fun shl/1, eval},
		{"sdrop", fun sdrop/1, eval},
		{"serase", fun serase/1, eval},
		{"sshow", fun sshow/1, eval},
		{"maths", gen_eval(fun math/1), user},
		{"math", gen_eval(fun math/1), user},
		{"sym", fun sym/1, user},
		{"lsym", fun lsym/1, host}
	].

bash(#{reply:=Reply, ping:=Ping, params:=[Param]}) ->
	util:unicode_os_putenv("args", Param),
	%{irc, {msg, {Reply, io_lib:format("~s~99999p", [Ping, util:safe_os_cmd("bash -c \"$args\"")])}}}.
	Result = util:safe_os_cmd("bash -c \"$args\""),
	{irc, {msg, {Reply, io_lib:format("~s~s", [Ping, re:replace(Result, "[\r\n]", [3,"14; ",3,2,2], [global])])}}}.

py(#{reply:=Reply, ping:=Ping, params:=[Param]}) ->
	util:unicode_os_putenv("script", Param),
	{irc, {msg, {Reply, io_lib:format("~s~99999p", [Ping, util:safe_os_cmd("python -c \"$script\"")])}}}.

bashl(#{reply:=Reply, ping:=Ping, params:=[Param]}) ->
	util:unicode_os_putenv("args", Param),
	RRaw = util:safe_os_cmd("bash -c \"$args\""),
	lines2channel(Reply, Ping, "", RRaw).

bashld(#{reply:=Reply, ping:=Ping, params:=[Param]}) ->
	util:unicode_os_putenv("args", Param),
	RRaw = util:safe_os_cmd("bash -c \"$args\""),
	lines2channel(Reply, [Ping,"`|"], "`", RRaw).

pyl(#{reply:=Reply, ping:=Ping, params:=[Param]}) ->
	util:unicode_os_putenv("script", Param),
	RRaw = util:safe_os_cmd("python -c \"$script\""),
	lines2channel(Reply, Ping, "", RRaw).

lines2channel(Reply, Pre, Post, RRaw) ->
	MaxLines = 5,
	R = lists:flatmap(fun
			(10) -> [10];
			(9) -> "    ";
			(T) -> [T]
		end, RRaw),
	L = case lists:flatmap(fun(T) -> case string:strip(T) of [] -> []; S -> [S] end end, string:tokens(R, "\n")) of
		X when length(X) =< MaxLines -> X;
		X ->
			File = io_lib:format("~32.16.0b", [binary:decode_unsigned(crypto:hash(md5, R))]),
			Filename = io_lib:format("/home/bot32/www/~s.txt", [File]),
			file:write_file(Filename, R),

			lists:sublist(X, MaxLines-1) ++ [io_lib:format("[~b lines omitted, see https://gn32.uk/admin/~s.txt for full output]", [length(X) - MaxLines + 1, File])]
	end,
	
	lists:foreach(fun
			(T) ->
				case string:strip(T) of
					[] -> ok;
					_ -> core ! {irc, {msg, {Reply, [Pre, T, Post]}}}
				end
		end, L).


eecho(Params=#{params:=Tokens}) ->
	ES = gen_eval_str(fun eval/1),
	NewParams = string:tokens(lists:flatten(["\"", string:join(Tokens, " "), "\""]), " "),
	ES(Params#{ping := [], params := NewParams}).

sym(#{reply:=RT, ping:=P, params:=Par}) ->
	os:putenv("sym", string:join(Par, " ")),
	{irc, {msg, {RT, [P, os:cmd("./sympy_eval.sh 5 \"$sym\"")]}}}.
lsym(#{reply:=RT, ping:=P, params:=Par}) ->
	os:putenv("sym", string:join(Par, " ")),
	{irc, {msg, {RT, [P, os:cmd("./sympy_eval.sh 60 \"$sym\"")]}}}.

shl(#{reply:=RT, ping:=P, params:=Params}) ->
	PStr = lists:flatten(string:join(Params, " ")),
	RString = case lists:last(PStr) of
		$. -> PStr;
		_ -> PStr ++ "."
	end,
	Bindings = config:get_value(data, [eval, shell], []),
	String = util:utf8_chars(list_to_binary(RString)),
	case erl_scan:string(String) of
		{ok, Tokens, _} ->
			case erl_parse:parse_exprs(Tokens) of
				{ok, Forms} ->
					case catch erl_eval:exprs(Forms, Bindings) of
						{value, Value, NewBinds} ->
							config:set_value(data, [eval, shell], NewBinds),
							{irc, {msg, {RT, [P, util:fix_utf8(io_lib:format("~99999p", [Value]))]}}};
						{'EXIT', {Reason, Stack}} when is_list(Stack) -> {irc, {msg, {RT, [P, format_reasonstack(Reason, Stack)]}}};
						{'EXIT', Term} -> {irc, {msg, {RT, [P, io_lib:format("Code exited with ~p", [Term])]}}};
						Term -> {irc, {msg, {RT, [P, io_lib:format("Code threw ~p", [Term])]}}}
					end;
				T -> {irc, {msg, {RT, [P, io_lib:format("~p", [T])]}}}
			end;
		T -> {irc, {msg, {RT, [P, io_lib:format("~p", [T])]}}}
	end.

sshow(#{reply:=RT, ping:=P}) ->
	case config:get_value(data, [eval, shell]) of
		'$none' -> {irc, {msg, {RT, [P, "No state found."]}}};
		V -> {irc, {msg, {RT, [P, io_lib:format("~99999p", [V])]}}}
	end.

serase(#{reply:=RT, ping:=P, params:=A}) when length(A) /= 1 -> {irc, {msg, {RT, [P, "Provide a single var name."]}}};
serase(#{reply:=RT, ping:=P, params:=[Var]}) ->
	Msg = case config:get_value(data, [eval, shell]) of
		'$none' -> "No vars found.";
		Vars ->
			Atom = list_to_atom(Var),
			case orddict:find(Atom, Vars) of
				{ok, Val} ->
					config:set_value(data, [eval, shell], orddict:erase(Atom, Vars)),
					io_lib:format("Dropped value ~99999p.", [Val]);
				error ->
					io_lib:format("No variable ~s found.", [Var])
			end
	end,
	{irc, {msg, {RT, [P, Msg]}}}.

sdrop(#{reply:=RT, ping:=P}) ->
	config:set_value(data, [eval, shell], []),
	{irc, {msg, {RT, [P, "State dropped."]}}}.

gen_eval(Func) ->
	fun(#{reply:=ReplyTo,ping:=Ping,params:=[    ]}) -> {irc, {msg, {ReplyTo, [Ping, "Provide a string to evaluate!"]}}};
	   (#{reply:=ReplyTo,ping:=Ping,params:=Params}) ->
		Raw = lists:flatten(string:join(Params, " ")),
		Str = case lists:last(Raw) of
			$. -> Raw;
			_ -> Raw ++ "."
		end,
		case catch Func(Str) of
			{ok, Value} -> {irc, {msg, {ReplyTo, [Ping, re:replace(io_lib:format("~w", [Value]), "[\r\n]", "")]}}};
			{'EXIT', {Reason, Stack}} when is_list(Stack) -> {irc, {msg, {ReplyTo, [Ping, format_reasonstack(Reason, Stack)]}}};
			{'EXIT', Term} -> {irc, {msg, {ReplyTo, [Ping, io_lib:format("Code exited with ~p", [Term])]}}};
			{cerr, Term} -> {irc, {msg, {ReplyTo, [Ping, Term]}}};
			Term -> {irc, {msg, {ReplyTo, [Ping, io_lib:format("Code threw ~p", [Term])]}}}
		end
	end.

gen_eval_str(Func) ->
	fun(#{reply:=ReplyTo,ping:=Ping,params:=[    ]}) -> {irc, {msg, {ReplyTo, [Ping, "Provide a string to evaluate!"]}}};
	   (#{reply:=ReplyTo,ping:=Ping,params:=Params}) ->
		Raw = lists:flatten(string:join(Params, " ")),
		Str = case lists:last(Raw) of
			$. -> Raw;
			_ -> Raw ++ "."
		end,
		case catch Func(Str) of
			{ok, Value} -> {irc, {msg, {ReplyTo, [Ping, re:replace(util:fix_utf8(io_lib:format("~s", [Value])), "[\r\n]", "")]}}};
			{'EXIT', {Reason, Stack}} when is_list(Stack) -> {irc, {msg, {ReplyTo, [Ping, format_reasonstack(Reason, Stack)]}}};
			{'EXIT', Term} -> {irc, {msg, {ReplyTo, [Ping, io_lib:format("Code exited with ~p", [Term])]}}};
			{cerr, Term} -> {irc, {msg, {ReplyTo, [Ping, Term]}}};
			Term -> {irc, {msg, {ReplyTo, [Ping, io_lib:format("Code threw ~p", [Term])]}}}
		end
	end.

format_reasonstack(Reason, [TopFrame|_]) ->
	io_lib:format("Error: ~p at ~p", [Reason, TopFrame]).

eval(RString) ->
	String = util:utf8_chars(list_to_binary(RString)),
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
	case re:run(String, "(^|[^a-zA-Z])fun($|[^a-zA-Z])", [{capture, none}]) of
		match -> {cerr, "Characters 'fun' detected in your input; quit trying to mess with this."};
		nomatch ->
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
			end
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
