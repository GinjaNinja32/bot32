-module(todo).
-compile(export_all).

get_commands() ->
	[
		{"todo", fun todo/1, user}
	].

get_help("todo") ->
	[
		"todo <done|notdone|del|delete|show> <ID>",
		"todo <Todo>"
	];
get_help(_) -> unhandled.

valid(LNick, N) ->
	config:get_value(data, [?MODULE, user, LNick, todo, N]) /= '$none'.
%	Next = config:get_value(data, [?MODULE, user, LNick, next]),
%	N < Next andalso N >= 1.

set_done(Nick, Reply, Ping, StrN, State) ->
	LNick = string:to_lower(Nick),
	N = list_to_integer(StrN),
	case valid(LNick, N) of
		true ->
			config:set_value(data, [?MODULE, user, LNick, todo, N, done], State),
			{irc, {msg, {Reply, [Ping, "Done."]}}};
		_ -> {irc, {msg, {Reply, [Ping, "Invalid entry."]}}}
	end.

show(Nick, Reply, Ping, StrN) ->
	LNick = string:to_lower(Nick),
	N = list_to_integer(StrN),
	case valid(LNick, N) of
		true ->
			State = case config:get_value(data, [?MODULE, user, LNick, todo, N, done]) of
				true -> " (DONE): ";
				false -> " (----): "
			end,
			Todo = config:get_value(data, [?MODULE, user, LNick, todo, N, entry]),
			{irc, {msg, {Reply, [Ping, integer_to_list(N), State, Todo]}}};
		_ -> {irc, {msg, {Reply, [Ping, "Invalid entry."]}}}
	end.

show_todos(Nick, Reply, Ping, All) ->
	LNick = string:to_lower(Nick),
	Todos = config:get_value(data, [?MODULE, user, LNick, todo]),
	case case All of
		true -> Todos;
		false -> lists:filter(fun({_,T}) -> not orddict:fetch(done, T) end, Todos)
	end of
		[] when All -> {irc, {msg, {Reply, [Ping, "You have no todos."]}}};
		[] -> {irc, {msg, {Reply, [Ping, "You have no active todos."]}}};
		TodoList ->
			lists:foreach(fun
					({ID,[{done,Done},{entry,T}]}) ->
						if
							Done -> core ! {irc, {msg, {Nick, [integer_to_list(ID), " (DONE): ", T]}}};
							All  -> core ! {irc, {msg, {Nick, [integer_to_list(ID), " (----): ", T]}}};
							true -> core ! {irc, {msg, {Nick, [integer_to_list(ID), ": ", T]}}}
						end
				end, TodoList)
	end.

delete(Nick, Reply, Ping, StrN) ->
	LNick = string:to_lower(Nick),
	N = list_to_integer(StrN),
	case valid(LNick, N) of
		true ->
			config:del_value(data, [?MODULE, user, LNick, todo, N]),
			{irc, {msg, {Reply, [Ping, "Deleted."]}}};
		_ -> {irc, {msg, {Reply, [Ping, "Invalid entry."]}}}
	end.

todo(P=#{nick:=Nick, reply:=Reply, ping:=Ping, params:=[Opt, StrN]}) ->
	case Opt of
		"done" -> set_done(Nick, Reply, Ping, StrN, true);
		"notdone" -> set_done(Nick, Reply, Ping, StrN, false);
		"show" -> show(Nick, Reply, Ping, StrN);
		"delete" -> delete(Nick, Reply, Ping, StrN);
		"del" -> delete(Nick, Reply, Ping, StrN);
		_ -> add(P)
	end;

todo(#{nick:=Nick, reply:=Reply, ping:=Ping, params:=[]}) ->
	show_todos(Nick, Reply, Ping, false);

todo(#{nick:=Nick, reply:=Reply, ping:=Ping, params:=["list"]}) ->
	show_todos(Nick, Reply, Ping, false);

todo(#{nick:=Nick, reply:=Reply, ping:=Ping, params:=["all"]}) ->
	show_todos(Nick, Reply, Ping, true);

todo(P) -> add(P).

add(#{nick:=Nick, reply:=Reply, ping:=Ping, params:=P}) ->
	LNick = string:to_lower(Nick),
	ID = case config:mod_get_value(data, [?MODULE, user, LNick, next], fun('$none') -> 2; (T) -> T+1 end) of
		'$none' -> 1;
		T -> T
	end,

	config:set_value(data, [?MODULE, user, LNick, todo, ID], [{done,false}, {entry,string:join(P," ")}]),
	{irc, {msg, {Reply, [Ping, "Inserted as entry ", integer_to_list(ID), "."]}}}.
