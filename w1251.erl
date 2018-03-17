-module(w1251).
-export([encode/1, decode/1]).

mapping() -> [
	{16#0402, 2}, % 80
	{16#201A, 1},
	{16#0453, 1},
	{16#201E, 1},
	{16#2026, 1},
	{16#2020, 2},
	{16#20AC, 1}, % 88
	{16#2030, 1},
	{16#0409, 1},
	{16#2039, 1},
	{16#040A, 1},
	{16#040C, 1},
	{16#040B, 1},
	{16#040F, 1},
	{16#0452, 1}, % 90
	{16#2018, 2},
	{16#201C, 2},
	{16#2022, 1},
	{16#2013, 2},
	{16#0000, 1}, % 98
	{16#2122, 1},
	{16#0459, 1},
	{16#203A, 1},
	{16#045A, 1},
	{16#045C, 1},
	{16#045B, 1},
	{16#045F, 1},
	{16#00A0, 1}, % A0
	{16#0000, 1},
	{16#040E, 1},
	{16#045E, 1},
	{16#0408, 1},
	{16#00A4, 1},
	{16#0490, 1},
	{16#00A6, 2},
	{16#0401, 1}, % A8
	{16#00A9, 1},
	{16#0404, 1},
	{16#00AB, 4},
	{16#0407, 1},
	{16#00B0, 2}, % B0
	{16#0406, 1},
	{16#0456, 1},
	{16#0491, 1},
	{16#00B5, 3},
	{16#0451, 1}, % B8
	{16#2116, 1},
	{16#0454, 1},
	{16#00BB, 1},
	{16#0458, 1},
	{16#0405, 1},
	{16#0455, 1},
	{16#0457, 1},
	{16#0410, 64} % C0-FF
].

encode_single(Codepoint) ->
	encode_single(Codepoint, 16#7F, mapping()).

encode_single(_, _, []) -> $?;
encode_single(Codepoint, N, [{Point, Count}|_]) when Point =< Codepoint andalso Codepoint < Point+Count ->
	N + (Codepoint - Point);
encode_single(Codepoint, N, [{Point, Count}|Rest]) ->
	logging:log(info, ?MODULE, "~p at ~p is not ~p:~p, moving on with ~p", [Codepoint, N, Point, Count, N+Count]),
	encode_single(Codepoint, N+Count, Rest).

decode_single(Byte) ->
	decode_single(Byte - 16#7F, mapping()).

decode_single(_, []) -> 16#FFFD; % U+FFFD REPLACEMENT CHARACTER
decode_single(Offset, [{Point, Count}|_]) when Offset < Count ->
	Point + Offset;
decode_single(Offset, [{Point, Count}|Rest]) ->
	logging:log(info, ?MODULE, "~p is not at ~p:~p, moving on with ~p", [Offset, Point, Count, Offset-Count]),
	decode_single(Offset - Count, Rest).

encode(UTF8) when is_list(UTF8) ->
	binary_to_list(encode(list_to_binary(UTF8)));
encode(UTF8) ->
	encode(UTF8, <<>>).

encode(<<>>, E) -> E;
encode(<<T/utf8, R/binary>>, E) when T < 16#7F ->
	encode(R, <<E/binary, T:8>>);
encode(<<T/utf8, R/binary>>, E) ->
	V = encode_single(T),
	encode(R, <<E/binary, V:8>>).

decode(W1251) when is_list(W1251) ->
	binary_to_list(decode(list_to_binary(W1251)));
decode(W1251) ->
	decode(W1251, <<>>).

decode(<<>>, E) -> E;
decode(<<T:8, R/binary>>, E) when T < 16#7F ->
	decode(R, <<E/binary, T/utf8>>);
decode(<<T:8, R/binary>>, E) ->
	V = decode_single(T),
	decode(R, <<E/binary, V/utf8>>).
