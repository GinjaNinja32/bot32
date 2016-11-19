-module(json).
-export([traverse/2, parse/1, write/1]).

traverse(error, _) -> error;
traverse(JSON, []) -> JSON;
traverse({struct,T}, [struct|Path]) -> traverse(T, Path);
traverse({array,T}, [array|Path]) -> traverse(T, Path);
traverse(Dict, [Key|Path]) ->
	case lists:keyfind(Key, 1, Dict) of
		false -> error;
		{_,V} -> traverse(V, Path)
	end.

parse(JSON) ->
	mochijson:decode(JSON).

write(JSON) ->
	mochijson:encode(JSON).
