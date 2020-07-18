-module(roman).
-compile(export_all).

get_commands() ->
	[
		{"roman", fun roman/1, [short], user}
	].

roman(#{reply:=Reply, ping:=Ping, params:=[Roman]}) ->
	case {valuable(Roman), romanisable(Roman)} of
		{true,_} ->
			Value = value(string:to_upper(Roman)),
			{irc, {msg, {Reply, [Ping, io_lib:format("~b", [Value])]}}};
		{_,true} ->
			Value = list_to_integer(Roman),
			if
				Value > 10000 ->
					{irc, {msg, {Reply, [Ping, "Be sensible."]}}};
				true ->
					R = romanise(Value),
					{irc, {msg, {Reply, [Ping, R]}}}
			end;
		_ ->
			{irc, {msg, {Reply, [Ping, "I couldn't figure out what to do with '", Roman, "'."]}}}
	end.
		

valuable(X) ->
	lists:all(fun(T) -> lists:member(T, "IVXLCDM") end, string:to_upper(X)).

value([]) -> 0;
value("I") -> 1;
value("V") -> 5;
value("X") -> 10;
value("L") -> 50;
value("C") -> 100;
value("D") -> 500;
value("M") -> 1000;
value([_]) -> invalid;
value([A,B|R]) ->
	AV = value([A]),
	BV = value([B]),
	if
		AV < BV -> % subtraction
			BV - AV + value(R);
		true ->
			AV + value([B | R])
	end.

romanisable(X) ->
	lists:all(fun(T) -> lists:member(T, "0123456789") end, X).

romanise(0) -> [];

romanise(1) -> "I";
romanise(4) -> "IV";
romanise(5) -> "V";
romanise(9) -> "IX";

romanise(10) -> "X";
romanise(40) -> "XL";
romanise(50) -> "L";
romanise(90) -> "XC";

romanise(100) -> "C";
romanise(400) -> "CD";
romanise(500) -> "D";
romanise(900) -> "CM";

romanise(1000) -> "M";

romanise(N) ->
	HighestFactor = if
		N > 1000 -> 1000;
		true ->
			NStr = integer_to_list(N),
			list_to_integer(lists:flatten([
				if
					hd(NStr) =< $3 -> $1;
					hd(NStr) =< $4 -> $4;
					hd(NStr) =< $8 -> $5;
					hd(NStr) =< $9 -> $9
				end,
				lists:duplicate(length(NStr)-1, $0)
			]))
	end,
	romanise(HighestFactor) ++ romanise(N - HighestFactor).


romanise2(N) ->
	case lists:foldl(fun({V, S}, {C, Str}) ->
		{C rem V, Str ++ lists:duplicate(C div V, S)} end,
		{N, ""},
		[                                         {1000, "M"},
		 { 900, "CM"}, { 500, "D"}, { 400, "CD"}, { 100, "C"},
		 {  90, "XC"}, {  50, "L"}, {  40, "XL"}, {  10, "X"},
		 {   9, "IX"}, {   5, "V"}, {   4, "IV"}, {   1, "I"}]) of
		{0, S} -> lists:flatten(S);
		{N, _} -> "failed: " ++ integer_to_list(N)
	end.
