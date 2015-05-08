-module(byond).
-export([send/3]).

send(Addr, Port, Msg) ->
	case gen_tcp:connect(Addr, Port, [binary, {packet, raw}, {active, false}], 30 * 1000) of
		{ok, Sock} ->
			case gen_tcp:send(Sock, encode(Msg)) of
				ok ->
					case recv(Sock) of
						{ok, V} -> parse(V);
						T -> {error, T}
					end;
				T -> {error, T}
			end;
		T -> {error, T}
	end.

encode(Msg) ->
	case binary:encode_unsigned(6+length(Msg)) of
		<<A:8, B:8>> -> ok;
		<<B:8>> -> A=0
	end,
	[0, 131, A, B, 0, 0, 0, 0, 0, Msg, 0].

recv(Sock) ->
	case {ok, <<_:16, Len:16, _:8>>} = gen_tcp:recv(Sock, 5) of
		{ok, <<_:16, Len:16, _:8>>} ->
			case gen_tcp:recv(Sock, Len-1) of
				{ok, S} -> {ok, lists:reverse(tl(lists:reverse(erlang:binary_to_list(S))))};
				T -> {error, T}
			end;
		T -> {error, T}
	end.

parse(Str) ->
	lists:foldl(fun(T, Dict) -> 
			case string:tokens(T, "=") of
				["players", V] -> orddict:store("players", decode(V), Dict);
				[[$p,$l,$a,$y,$e,$r|_],V] ->
					Players = orddict:fetch("playerlist", Dict),
					orddict:store("playerlist", [decode(V)|Players], Dict);
				[K,V] -> orddict:store(decode(K), decode(V), Dict);
				[K] -> orddict:store(decode(K), "?", Dict)
			end
		end, orddict:store("playerlist", [], orddict:new()), string:tokens(Str, "&;")).

decode(V) -> http_uri:decode(V).
