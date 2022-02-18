% Author: Martin Pilat
% Date: 6. 3. 2017
% Last Update: 4. 3. 2020

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			3. cviceni -- Seznamy, aritmetika
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Minule jsme se venovali seznamum, dnes v tom budeme jeste pokracovat, pridame 
% ale i aritmetiku.

% Zacneme jednoduchym prikladem na odebrani prvku ze seznamu.

% vymaz(+X, +X, -V) :- vymaze vyskyt prvku X ze seznamu S
%                      a vrati vysledny seznam jako V
vymaz(X,[X|Xs],Xs).
vymaz(X,[Z|Xs],[Z|Y]):-vymaz(X,Xs,Y).

% Obcas se hodi, aby procedura uspela i v pripade, ze seznam dany prvek 
% neobsahuje

% vymaz1(X,Odkud,CoZbude) :- totez co vymaz
%                            jen uspeje i kdyz X neni prvkem seznamu Odkud.
vymaz1(_,[],[]).
vymaz1(X,[X|T],T):-!.
vymaz1(X,[H|T],[H|T1]) :- vymaz1(X,T,T1).

% Vsimnete si, ze pokud nenapisete `!` na konec prvni klauzule, Prolog vraci i 
% seznamy, ve kterych jsou odebrane jine vyskyty nez ten prvni a nakonec vrati i
% puvodni seznam. `!` je operator rezu a rika Prologu, ze zvolena rozhodnuti ma 
% povazovat za konecna. Zakazuje mu backtrackovat v danem miste - orezava 
% vsechny vetve vypoctu krome te prvni, proto ten nazev "operator rezu".

% Zkusme si jeste napsat proceduru, ktera vrati prostredni prvek ze seznamu. Jak 
% ji napisete, kdyz nevite dopredu, jak je seznam dlouhy?

prostredni(S,X):- prostredni(S,S,X). 
prostredni([_],[X|_],X). 
prostredni([_,_],[X|_],X). 
prostredni([_,_|T1],[_|T2],X):- prostredni(T1,T2,X).

% Jak procedura funguje? Vsimnete si, ze v prvnim seznamu se jde vzdy po dvou a 
% ve druhem po jednom kroku. Kdyz se tedy prvni seznam vyprazdni (ma max 2 
% prvky), na zacatku toho druheho je presne prostredni prvek. 

% Nakonec napisme jeste proceduru, ktera spocita prunik dvou seznamu (ta je tezsi
% a potrebujete na ni operator rezu). 

prunik([],_,[]).
prunik(_,[],[]).
prunik([X|Xs], Y, [X|Z]):-member(X,Y), prunik(Xs,Y,Z), !.
prunik([_|Xs], Y, Z):-prunik(Xs,Y,Z). 

% __Ukol__: Zvladli byste napsat sjednoceni? Zkuste si to.

% Podivejme se nyni na aritmetiku v Prologu a na pocitani. Pocitani v Prologu 
% ma sva uskali a lisi se od pocitani v jazycich, na jake jste zvykli. Cast 
% techto problemu plyne z toho, ze v Prologu neexistuje operator prirazeni, `=`
% dela unifikaci. Misto `=` se v aritmetickych vypoctech v Prologu pouziva 
% operator `is`.

% Rozdil mezi `=` a `is` je videt na nasledujicich dotazech a odpovedich Prologu.

% ? X = 1+2
% X = 1+2.

% ? X is 1+2
% X = 3.

% V Prologu take existuji obvykle relacni operatory: rovna se se zapise jako 
% `=:=`, nerovna se jako `=\=`, mensi nez a vetsi nez jako `<` a `>`, pripadne 
% mensi nebo rovno a vetsi nebo rovno jako `=<` a `>=`. Davejte si pozor na to,
% ze nektere z techto operatoru se zapisuji jinak, nez jste asi zvykli.

% U vsech relacnich operatoru je potreba, aby obe strany vyrazu uz mely
% prirazenu konkretni hodnotu. U operatoru `is` staci, kdyz je prirazena
% konkretni hodnota termum na prave strane (pokud je prirazena konkretni hodnota
% i termum na leve strane, operator uspeje, pokud se obe hodnoty rovnaji, 
% jinak selze). 

% S pomoci aritmetiky muzeme napsat nekolik jednoduchych procedur. Nejjednodussi 
% z nich je vypocet delky seznamu

% delka(+S,-L) :- do L dosadi delku seznamu S
delka([], 0).
delka([_|T], N):-delka(T,N1),N is N1 + 1.

% Vsimnete si, ze vypocet `N` a rekurzivni volani nejde prohodit, protoze jinak 
% `N1` jeste nema prirazenou zadnou hodnotu.

% Muzeme take napsat jednoduchou proceduru pro vypocet druhe mocniny zadaneho 
% cisla.

% nadruhou(+X, -Y) :- prirazuje do Y druhou mocninu X
nadruhou(X,Y):-Y is X*X.

% Zajimavejsi ale je napsat proceduru, ktera spocita libovolnou mocninu 
% libovolneho cisla.

% mocnina(+Z,+M,-V) :- pocita Z^M a vysledek ulozi do V
mocnina(_,M,_):-M<0,fail.
mocnina(_,0,1).
mocnina(Z,1,Z).
mocnina(Z,2,V):-nadruhou(Z,V).
mocnina(Z,M,V):-M>2, M1 is M - 1, mocnina(Z,M1,V1), V is V1*Z.

% Za pouziti aritmetiky se daji napsat i dalsi procedury pro praci se seznamy, 
% napriklad nalezeni minima v seznamu. Vsimnete si tady i pouziti akumulatoru.

% minseznam(+S, -M) :- najde minimum v seznamu S a ulozi ho do M
minseznam([H|T], M) :- minseznam(T, H, M).
minseznam([], A, A).
minseznam([H|T], A, M):-A=<H,minseznam(T,A,M).
minseznam([H|T], A, M):-H<A,minseznam(T,H,M).

% Muzeme napsat i pristup k `N`-temu prvku v seznamu. Dejte si ale pozor,
% pristup trva linearne dlouho.

% nty(+N, +S, +X) :- do X ulozi N-ty prvek seznamu S
nty(1, [H|_], H).
nty(N, [_|T], X):-N1 is N-1,nty(N1,T,X).