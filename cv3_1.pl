prvek(X, [X|_]).
prvek(X, [_|S]):-prvek(X, S).

pricti1([],[]).
pricti1([X|Xs], [X1|Zs]):-X1 is X + 1, pricti1(Xs, Zs).

%map(+S, +F, -FS)
map([], _, []).
map([X|Xs], F, [Y|Ys]):-call(F, X, Y),map(Xs, F, Ys).

add1(X, Y):-Y is X + 1.
div2(X, Y):-Y is X / 2.
gen(0, []).
gen(N, [1|Xs]):-N>0, N1 is N - 1, gen(N1, Xs).

genlist([], []).
genlist([X|Xs], [Y|Ys]):-gen(X, Y), genlist(Xs, Ys).

soucet1([], 0).
soucet1([X|Xs], N):-soucet1(Xs, N1), N is X + N1.

concat1([], []).
concat1([X|Xs], N):-concat1(Xs, N1), append(X, N1, N).

soucet(X, S):-soucet(X, 0, S).
soucet([], A, A).
soucet([X|Xs], A, S):-A1 is A + X, soucet(Xs, A1, S).

concat(X, S):-concat(X, [], S).
concat([], A, A).
concat([X|Xs], A, S):-append(A, X, A1), concat(Xs, A1, S).

fold([], _, A, A).
fold([X|Xs], F, A, R):-call(F, A, X, A1), fold(Xs, F, A1, R).