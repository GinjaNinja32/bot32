-module(byond).
-export([send/3, params2dict/1, dict2params/1, vencode/1, vdecode/1]).

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
	case binary:encode_unsigned(6+lists:flatlength(Msg)) of
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
				["players", V] -> orddict:store("players", vdecode(V), Dict);
				[[$p,$l,$a,$y,$e,$r|_],V] ->
					case orddict:find("playerlist", Dict) of
						{ok, Players} -> orddict:store("playerlist", [vdecode(V)|Players], Dict);
						error -> orddict:store("playerlist", [vdecode(V)], Dict)
					end; 
				[K,V] -> orddict:store(vdecode(K), vdecode(V), Dict);
				[K] -> orddict:store(vdecode(K), "?", Dict)
			end
		end, orddict:new(), string:tokens(Str, "&;")).

vencode(V) -> http_uri:encode(V).
vdecode(V) -> http_uri:decode(re:replace(V, "\\+", " ", [{return, list}, global])).

params2dict(Params) ->
	lists:foldl(fun(T, Dict) ->
			case string:tokens(T, "=") of
				[K, V] -> orddict:store(vdecode(K), vdecode(V), Dict);
				[K] -> orddict:store(vdecode(K), none, Dict);
				X -> common:debug("BYOND", "params2list() encountered '~s', splitting to ~p", [T,X]), Dict
			end
		end, orddict:new(), string:tokens(Params, "&;")).

dict2params(Dict) ->
	orddict:fold(fun
			(K,none,Acc) -> [vencode(K), $& | Acc];
			(K,V,Acc) -> [vencode(K), $=, vencode(V), $& | Acc]
		end, [], Dict).
		
