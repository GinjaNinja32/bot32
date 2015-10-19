-module(config).

-export([
		is_started/1,
		start/1,
		start_transient/1,
		stop/1,
		get_value/2,
		require_value/2,
		get_value/3,
		set_value/3,
		offer_value/3
	]).

is_started(Name) ->
	case whereis(Name) of
		undefined -> false;
		_ ->
			Name ! {self(), ping},
			receive
				{Name, pong} -> true
			after
				500 -> false
			end
	end.

start(Name) -> spawn(fun() -> init(Name, [], true) end).
start_transient(Name) -> spawn(fun() -> init(Name, [], false) end).

stop(Name) ->
	Name ! {self(), stop},
	receive
		{Name, stop} -> ok
	end.

get_value(Name, Key) -> get_value(Name, Key, '$none').
get_value(Name, Key, Default) ->
	Name ! {self(), get, Key, Default},
	receive
		{Name, get, Key, Value} -> Value;
		{Name, error} -> error
	end.

require_value(Name, Key) ->
	case get_value(Name, Key) of
		'$none' -> throw({required_key_not_found,Key});
		Value -> Value
	end.

set_value(Name, Key, Value) ->
	Name ! {self(), set, Key, Value},
	receive
		{Name, set, Key} -> ok;
		{Name, error} -> error
	end.

offer_value(Name, Key, Value) -> % set iff not currently set
	Name ! {self(), offer, Key, Value},
	receive
		{Name, offer, Key} -> ok;
		{Name, error} -> error
	end.

init(Name, Config, SaveToFile) ->
	register(Name, self()),
	loop(Name, Config, SaveToFile),
	unregister(Name).

try_load_key(Name, Key, Config) ->
	Filename = ["./", atom_to_list(Name), "/", atom_to_list(Key), ".crl"],
	case file:consult(Filename) of
		{ok, [Value]} -> orddict:store(Key, Value, Config);
		T -> logging:log(info, ?MODULE, "Tried to load ~s: ~p", [Filename, T]), orddict:store(Key, [], Config)
	end.

loop(Name, Config, SaveToFile) ->
	receive
		{Pid, get, Key, Default} ->
			NewConf = try
				NC = case orddict:find(hd(Key), Config) of
					error -> if SaveToFile -> try_load_key(Name, hd(Key), Config); true -> Config end;
					{ok,_} -> Config
				end,
				Pid ! {Name, get, Key, get_raw(Key, Default, NC)},
				NC
			catch
				A:B ->
					logging:log(error, ?MODULE, "caught ~p:~p while fetching key ~p", [A,B,Key]),
					Pid ! {Name, error},
					Config
			end,
			loop(Name, NewConf, SaveToFile);
		{Pid, set, Key, Value} ->
			Config1 = case orddict:find(hd(Key), Config) of
				error -> if SaveToFile -> try_load_key(Name, hd(Key), Config); true -> Config end;
				{ok, _} -> Config
			end,
			NewConf = try
				NC = set_raw(Key, Value, Config1),
				if
					SaveToFile -> save(Name, hd(Key), NC);
					true -> ok
				end,
				Pid ! {Name, set, Key},
				NC
			catch
				A:B ->
					logging:log(error, ?MODULE, "caught ~p:~p while setting key ~p", [A,B,Key]),
					Pid ! {Name, error},
					Config1
			end,
			loop(Name, NewConf, SaveToFile);
		{Pid, offer, Key, Value} ->
			Config1 = case orddict:find(hd(Key), Config) of
				error -> try_load_key(Name, hd(Key), Config);
				{ok, _} -> Config
			end,
			NewConf = try
				NC = offer_raw(Key, Value, Config1),
				if
					SaveToFile -> save(Name, hd(Key), NC);
					true -> ok
				end,
				Pid ! {Name, offer, Key},
				NC
			catch
				A:B ->
					logging:log(error, ?MODULE, "caught ~p:~p while offering key ~p", [A,B,Key]),
					Pid ! {Name, error},
					Config1
			end,
			loop(Name, NewConf, SaveToFile);
		{Pid, stop} ->
			Pid ! {Name, stop},
			ok;
		{Pid, ping} ->
			Pid ! {Name, pong},
			loop(Name, Config, SaveToFile)
	end.

save(Name, Key, Config) ->
	Filename = ["./", atom_to_list(Name), "/", atom_to_list(Key), ".crl"],
	case file:write_file(Filename, io_lib:format("~p.~n", [orddict:fetch(Key, Config)])) of
		ok -> ok;
		T -> logging:log(error, ?MODULE, "Error writing file ~s: ~p", [Filename, T])
	end.

get_raw([Key | Rest], Default, Config) ->
	case orddict:find(Key, Config) of
		{ok, V} ->
			if
				Rest == [] -> V;
				true -> get_raw(Rest, Default, V)
			end;
		error -> Default
	end.

set_raw([Key], Value, Config) -> orddict:store(Key, Value, Config);
set_raw([Key | Rest], Value, Config) ->
	NewV = case orddict:find(Key, Config) of
		{ok, V} -> set_raw(Rest, Value, V);
		error -> set_raw(Rest, Value, [])
	end,
	orddict:store(Key, NewV, Config).

offer_raw([Key], Value, Config) ->
	case orddict:find(Key, Config) of
		{ok, _} -> Config;
		error -> orddict:store(Key, Value, Config)
	end;
offer_raw([Key | Rest], Value, Config) ->
	NewV = case orddict:find(Key, Config) of
		{ok, V} -> offer_raw(Rest, Value, V);
		error -> offer_raw(Rest, Value, [])
	end,
	orddict:store(Key, NewV, Config).
