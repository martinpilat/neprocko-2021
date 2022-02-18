% Author: Martin Pilat
% Date: 4. 4. 2017
% Update: 17. 3. 2020

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			7. cviceni -- =.., SAT solver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Pohrajme si na rozehrati jeste jednou se stromy. Napisme proceduru, ktera
% vrati seznam, ve kterem bude binarni strom ulozeny po vrstvach.

% vrstvy(+T, -S) :- S je seznam, ktery vznikl ze stromu T tak, ze do nej byly 
%				    prvky dane po vrstvach
vrstvy(T, S):-vrstvy2([T|A]-A, S).
vrstvy2([A]-A,[]):-!.   %Rez je potreba, abysme nasli reseni jen jednou
vrstvy2([nil|A]-B, S):-vrstvy2(A-B,S).
vrstvy2([t(L,U,P)|A]-B, [U|S]):-B=[L,P|C],vrstvy2(A-C, S).

% V Prologu uz umime skoro vsechno. Zbylo nam jen par veci, se kterymi si
% pohrajeme dnes. Prvni z veci je operator `=..`. O tom jsme se sice jiz
% strucne zminili, ale poradne jsme se mu zatim nevenovali, protoze jsme to
% nestihli.

% Operator `=..` Term na leve strane prevadi na seznam, pripadne seznam na
% prave strane prevadi na term. Prvni polozka seznamu je vzdy funktor, na
% dalsich mistech potom jsou jednotlive parametry. Napr. na dotaz
%  `?- T =.. [f, x, y]`, dostanete odpoved `T = f(x,y)`, a naopak, na dotaz
% `?- f(x,y) =.. S`, dostanete odpoved `S = [f,x,y]`.

% Zkusme hned tento operator vyuzit a napsat substituci promennych v termech.
% Tedy, kdyz zadame na vstupu term `f(x,y)`, a chceme substituovat `z` za `x`,
% tak dostaneme `f(z,y)`. Zacneme napred s jednoduchymi termy, vnorene udelame
% pozdeji.

% substituceJTerm(+Term, +PromennaVTermu, +PromennaNova, -VysledkyTerm) :-
% 				VyslednyTerm je Term, ve kterem promenna PromennaVTermu byla nahrazena
%				promennou PromennaNova	
substituceJTerm(T, P, N, X) :- T =.. [F|V], substituceSeznam(V, P, N, X1), X =..[F|X1].

% Potrebujeme substituci v seznamech, kterou jsme psali na jednech z drivejsich
% cviceni.

%substituceSeznam(+S, +P, +N, -X) :- seznam X vznikne ze seznamu S nahrazenim vsech vyskytu
                                    P promennou N.
substituceSeznam([], _, _, []).
substituceSeznam([P|Ss], P, N, [N|S1]):-!,substituceSeznam(Ss,P,N,S1).
substituceSeznam([S|Ss], P, N, [S|S1]):-substituceSeznam(Ss,P,N,S1).

% Zkusme ted napsat substituci pro slozene termy.

% substituceTerm(+T, +P, +N, -X) :- term X vznikne z termu T tak, ze se kazdy vyskyt
%									P nahradi N
substituceTerm(T,P,N,X):-T =.. [F|S], substituceTerm2(S,P,N,X1), X =.. [F|X1]. %Odebereme funktor a zpracujeme zbytek parametru
substituceTerm2([],_,_,[]).	
substituceTerm2([T|Ts],P,N,[T1|X]):-T =.. S, S = [P], !, T1 =.. [N], substituceTerm2(Ts,P,N,X). %Parametr je jen jeden znak a je to ten, ktery chceme nahradit.
substituceTerm2([T|Ts],P,N,[T1|X]):-T =.. S, S = [_], !, T1 = T, substituceTerm2(Ts,P,N,X). %Parametr je jen jeden znak a neni to ten, ktery chceme nahradit.
substituceTerm2([T|Ts],P,N,[T1|Ts]):-substituceTerm(T,P,N,T1). %Parametr je vnoreny term.

% Na zaver jeste zkusime napsat SAT solver. Vstupem bude formule v CNF zadana 
% jako seznam klauzuli. Promenne jsou oznacene prirozenymi cisly, negovane 
% literaly jsou zaporna cisla (takze komplementarni literal k A je -A). Napr. 
% formule `((A nebo B nebo C) and (not A nebo B))` je zapsana jako 
% `[[1,2,3],[-1,2]]`. Cilem je napsat proceduru `sat/2`, ktera vraci vsechna
% ohodnoceni, ve kterych plati zadana vstupni formule, ohodnoceni je seznam
% dvojic `P-E`, kde `P` je promenna a `E` je jeji hodnota `(0, 1)`. Priklad
% vstupu a vystupu je nize.

% ?- sat([[1,2,3],[-2,4],[3]], A).
% A = [1-1, 2-1, 3-1, 4-1] ;
% A = [1-1, 2-0, 3-1, 4-1] ;
% A = [1-1, 2-0, 3-1, 4-0] ;
% A = [1-0, 2-1, 3-1, 4-1] ;
% A = [1-0, 2-0, 3-1, 4-1] ;
% A = [1-0, 2-0, 3-1, 4-0] ;
% false.

% sat(+F, -E):-E je ohodnoceni, ve kterem plati fromule F
sat(F, E):-sat(F, [], E).
sat([], E, E).
sat([C|F], A, E):-splnKlauzuli(C, A, E1), sat(F, E1, E).

% splnKlauzuli(C, A, E):-E je rozsireni ohodnoceni A takove, ze v nem plati klauzule C
splnKlauzuli(C, A, E):-ohodnotPromenne(C, A, E1), append(A, E1, E), platna(C, E).

% platna(C, E):-klauzule C je platna pri ohodnoceni E
platna([L|_], E):-L<0, Lc is -L, member(Lc-0, E), !.
platna([L|_], E):-L>0, member(L-1, E), !.
platna([_|Ls], E):-platna(Ls, E).

% ohodnotPromenne(C, A, E):-E je libovolne ohodnoceni promennych, ktere jeste nejsou 
%							ohodnocene v A
ohodnotPromenne([], _, []).
ohodnotPromenne([L | Ls], A, E):-L > 0, member(L-_, A), !, ohodnotPromenne(Ls, A, E).
ohodnotPromenne([L | Ls], A, E):-L < 0, Lc is -L, member(Lc-_, A), !, ohodnotPromenne(Ls, A, E).

ohodnotPromenne([L | Ls], A, [L-1|E1]):-L > 0, \+member(L-0, A), ohodnotPromenne(Ls, [L-1|A], E1).
ohodnotPromenne([L | Ls], A, [L-0|E1]):-L > 0, \+member(L-1, A), ohodnotPromenne(Ls, [L-0|A], E1).
ohodnotPromenne([L | Ls], A, [Lc-0|E1]):-L < 0, Lc is -L, \+member(Lc-1, A), ohodnotPromenne(Ls, [Lc-0|A], E1).
ohodnotPromenne([L | Ls], A, [Lc-1|E1]):-L < 0, Lc is -L, \+member(Lc-0, A), ohodnotPromenne(Ls, [Lc-1|A], E1).