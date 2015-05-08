-module(flatten).
-compile(export_all).

flatten(L) -> lists:reverse(flatten(L, [])).

flatten([], Out) -> Out;
flatten([A|B], Out) when is_list(A) -> flatten(B, flatten(A, Out));
flatten([A|B], Out) -> flatten(B, [A | Out]);
flatten(A, Out) -> [A | Out].
