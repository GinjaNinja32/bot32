-module(complex).
-compile({no_auto_import,[abs/1]}).
-compile(export_all).

-define(e, 2.718281828459).
-define(is_number(A), (is_integer(A) orelse is_float(A))).

is_complex({A,B}) when is_integer(A); is_float(A), is_integer(B); is_float(B) -> true;
is_complex(_) -> false.

decomplex({A,N}) when N == 0 -> A;
decomplex(A) -> A.

real({A,_}) -> A;
real(A) -> A.

imag({_,B}) -> B;
imag(_) -> 0.

conj({A,B}) -> {A,-B};
conj(A) -> A.

abs({A,B}) -> math:sqrt(A*A+B*B);
abs(A) when A < 0 -> -A;
abs(A) -> A.

arg({A,B}) -> math:atan2(B,A);
arg(A) when A < 0 -> math:pi();
arg(_) -> 0.

sqrt(Z={R,_}) ->
	Mod = abs(Z),
	decomplex({math:sqrt((Mod+R)/2),
	           math:sqrt((Mod-R)/2)});
sqrt(R) ->
	Mod = abs(R),
	decomplex({math:sqrt((Mod+R)/2),
	           math:sqrt((Mod-R)/2)}).

pow(Z={_,_}, Z2={_,_}) ->
	Mod = abs(Z),
	Arg = arg(Z),
	Pre = pow(Mod, Z2),
	Post = pow(?e, mul({0,Arg}, Z2)),
	mul(Pre, Post);

pow(R, Z={_,_}) ->
	Exponent = mul(Z, math:log(R)),
	ExpX = math:exp(real(Exponent)),
	Y = imag(Exponent),
	decomplex({math:cos(Y) * ExpX,
	           math:sin(Y) * ExpX});

pow(Z={_,_}, R) ->
	Mod = abs(Z),
	Arg = arg(Z),
	Root = math:pow(Mod, R),
	decomplex({math:cos(Arg*R) * Root,
	           math:sin(Arg*R) * Root});

pow(R, R2) when R < 0 -> pow({R,0}, R2);
pow(R, R2) -> math:pow(R, R2).

root(Z, N) -> pow(Z, 1/N).

% Arithmetic

add(A, B) when ?is_number(A) andalso ?is_number(B) -> A + B;
add(A, B) -> decomplex({real(A) + real(B), imag(A) + imag(B)}).

sub(A, B) when ?is_number(A) andalso ?is_number(B) -> A - B;
sub(A, B) -> decomplex({real(A) - real(B), imag(A) - imag(B)}).

mul(A, B) when ?is_number(A) andalso ?is_number(B) -> A * B;
mul(A, B) ->
	decomplex({real(A) * real(B) - imag(A) * imag(B),
	           real(A) * imag(B) + imag(A) * real(B)}).

divd(A, B) when ?is_number(A) andalso ?is_number(B) -> A / B;
divd(A, B) ->
	P = real(B)*real(B) + imag(B)*imag(B),
	case mul(A, conj(B)) of
		{X, Y} -> decomplex({X/P, Y/P});
		X -> X/P
	end.

