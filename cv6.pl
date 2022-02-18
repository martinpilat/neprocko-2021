% Author: Martin Pilat
% Date: 28. 3. 2017
% Updated: 23. 3. 2020

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			6. cviceni -- Grafy, prohledavani stavoveho prostoru
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Je cas zacit v Prologu take delat neco slozitejsiho. Uvidime, ze Prolog se da
% pouzit i k prohledavani grafu, hledani cest a podobne. 

% Jak ale v Prologu graf reprezentovat? Existuji asi dva zpusoby, ktere se daji
% relativne snadno pouzit. Jeden z nich je seznam nasledniku, druhy je seznam
% hran.

% Pokud chceme graf reprezentovat jako seznam nasledniku muzeme si napr.
% vytvorit predikat
% `grafSN([Vrchol1->[Naslednici1],Vrchol2->[Naslednici2], ...])`. Kdyz se  nam
% to hodi, muzeme si pridat i samostatny seznam vrcholu napr. jako
% `grafSN([Vrcholy], [V1->[N1], V2->[N2], ...])`. (Samozrejme, na operatoru,
% ktery pouzijete pro oddeleni vrcholu od seznamu vubec nezalezi.).

% Druhou moznosti, jak reprezentovat graf je pouzit seznam hran. V takovem
% pripade si vytvorime predikat
% `grafSH([Vrcholy], [hrana(V1,V2), hrana(V3,V4), ...])`, kde `V1`, `V2`, `V3`,
% `V4` jsou samozrejme vrcholy ze seznamu `[Vrcholy]`.

% Vytvorme si tedy popis maleho grafu o 5 vrcholech:
% `grafSN([1,2,3,4,5],[1->[2,3,4],2->[5],3->[1,2,4],4->[1],5->[2,3])`

% Prvni vec, ktera se nam hodi jsou procedury na prevod mezi obema
% reprezentacemi.

% gSNnaSH(+GrafSN,-GrafSH) :- GrafSH je GrafSN prevedeny ze seznamu nasledniku na seznam hran.
gSNnaSH(grafSN(V, N), grafSH(V, Hrany)):-bagof(hrana(V1,V2), E^(member(V1->E,N),member(V2,E)), Hrany).

% sSHnaSN(+GrafSH,-GrafSN) :- GrafSN je GrafSH prevedeny ze seznamu hran na seznam nasledniku
gSHnaSN(grafSH(V, E), grafSN(V, Naslednici)):-setof(V1->N, (member(V1,V),setof(X, member(hrana(V1,X),E),N)),Naslednici).

% Pouzivejme ted chvili grafy reprezentovane seznamy hran. Napred muzeme napsat
% predikat, ktery vyjadruje, ze mezi dvema vrcholy existuje cesta.

% cesta(+GrafSH, +Odkud, +-Kam) :- v grafu GrafSH existuje cesta z Odkud do Kam
cesta(grafSH(_,E), O, K) :- member(hrana(O,K), E).
cesta(grafSH(V,E), O, K) :- member(hrana(O,Z), E), cesta(grafSH(V,E), Z, K). 

% Procedura `cesta/3` ma nekolik problemu. Jeden z nich je, ze nevraci nalezenou
% cestu, druhy je, ze bude vracet uplne vsechny cesty, ktere v grafu existuji
% vcetne tech, ktere obsahuji cykly. Navic se muze i zacyklit (bude prochazet
% ten samy cyklus v grafu stale kolem dokola). Zkusme tedy napsat proceduru,
% ktera bude vracet vsechny cesty bez cyklu.

cestaBC(grafSH(V,E), O, K, C) :- cestaBC(grafSH(V,E),O,K,[],C).
cestaBC(_, O, O, _, [O]).
cestaBC(grafSH(V,E), O, K, P, [O|C]) :- member(hrana(O,Z), E), 
										\+member(Z, P), 
										cestaBC(grafSH(V,E), Z, K, [O|P], C). 

% Hledani cesty v grafu je podobne prohledavani stavoveho prostoru. Jen pri
% prohledavani stavoveho prostoru nemame tento prostor zadany explicitne seznamy
% nasledniku jednotlivych vrcholu, ale jen akcemi, ktere lze v kazdem stavu
% provest. Predstavme si napriklad znamy priklad se dvema nadobami a prelevanim
% vody mezi nimi. Mame dve nadoby, jednu o objemu `V1` litru, druhou o objemu
% `V2` litru. Navic mame k dispozici neomezeny zdroj vody a vodu muzeme z nadob
% libovolne vylevat. Cilem je najit takovou posloupnost prelevani, ktera povede
% k tomu, ze v druhe nadobe je prave dany objem vody.

% Stav muzeme v tomto pripade reprezentovat jako dvojici `s(X, Y)`, kde `X` je
% mnozstvi vody v prvni nadobe a `Y` je mnozstvi vody v druhe nadobe. Mame potom
%  nekolik akci, ktere muzeme v kazdem stavu provest.

% akce(+Stav, +MaxV1, +MaxV2, -NovyStav):-NovyStav je Stav, ktery vznikne
% aplikaci jedne akce na Stav, kdyz nadoby maji maximalni objemy MaxV1 a MaxV2
akce(s(_, Y), _, _, s(0, Y)). % vyliti prvni nadoby
akce(s(X, _), _, _, s(X, 0)). % vyliti druhe nadoby
akce(s(_, Y), V1, _, s(V1, Y)). % naplneni prvni nadoby
akce(s(X, _), _, V2, s(X, V2)). % naplneni druhe nadoby
akce(s(X, Y), V1, _, s(X1, Y1)):-X + Y > V1, X1 = V1, Y1 is Y - (V1 - X). % preliti druhe nadoby do prvni, nevejde se vse
akce(s(X, Y), V1, _, s(X1, Y1)):-X + Y =< V1, X1 is X + Y, Y1 = 0. % preliti druhe do prvni, vse se vejde 
akce(s(X, Y), _, V2, s(X1, Y1)):-X + Y > V2, Y1 = V2, X1 is X - (V2 - Y). % preliti prvni do druhe, nevejde se vse
akce(s(X, Y), _, V2, s(X1, Y1)):-X + Y =< V2, Y1 is X + Y, X1 = 0. % preliti prvni do druhe, vejde se vse

% Ted uz tedy umime ke kazdemu stavu najit vsechny sousedni stavy. Muzeme se 
% tedy podivat, jak bude stav vypadat po jednom kroku. Akce nam vlastne ukazuje 
% jeden krok, ktery muzeme provest. Jak bude vypadat stav po provedeni `N` kroku?

% nkroku(+Stav, +MaxV1, +MaxV2, +N, -NovyStav):-NovyStav je stav, ktery vznikne
% aplikovani N kroku na Stav, MaxV1 a MaxV2 jsou maximalni objemy nadob
nkroku(S, _, _, 0, S):-!. % Musime zariznout, jinak budeme zkouset zaporne pocty kroku
nkroku(S, MaxV1, MaxV2, N, S2):-akce(S, MaxV1, MaxV2, S1), 
                                N1 is N - 1, 
                                nkroku(S1, MaxV1, MaxV2, N1, S2).

% Predikat `nkroku` je uzitecny a funguje, ale obcas zkousi i cesty, na kterych
% se nekolikrat opakuje ten samy stav. Kdyz nas bude zajimat nejkratsi reseni,
% tak takove cesty zcela jiste nechceme a muzeme se jich zbavit. Navic by se
% nam libilo ziskat posloupnost stavu, ktere vedou k cilovemu. K tomu pouzijeme
% akumulator (a navic jednu vystupni promennou, ktera bude obsahovat
% posloupnost stavu).

% nkroku(+Stav, +MaxV1, +MaxV2, +N, -Posloupnost, -NovyStav):-NovyStav je stav,
% ktery vznikne aplikovani N kroku na Stav, MaxV1 a MaxV2 jsou maximalni objemy
% nadob, Posloupnost je posloupnost stavu ze Stav do NovyStav, ktera vede k 
% vytvoreni NovyStav, stavy v Posloupnosti se neopakuji
nkroku(S, MaxV1, MaxV2, N, P, S1):-nkroku(S, MaxV1, MaxV2, N, [], P, S1). 
nkroku(S, _, _, 0, A, P, S):-reverse(A, P).
nkroku(S, MaxV1, MaxV2, N, A, P, S2):-N>0, akce(S, MaxV1, MaxV2, S1), 
                                    \+member(S1, A),
                                    N1 is N - 1, 
                                    nkroku(S1, MaxV1, MaxV2, N1, [S1|A], P, S2).

% Pomoci predikatu `nkroku/6` uz muzeme napsat prohledavani, ktere nam zajisti, 
% ze najdeme nejkratsi reseni daneho problemu. Udelame tzv. iterovane
% prohlubovani. Na zacatku nastavime maximalni pocet kroku na 1, zkusime tak
% problem vyresit, kdyz se to nepovede, zvysime pocet kroku na 2 a opakujeme.
% Po kazdem neuspechu zvysime pocet kroku o 1.

% vyresID(+PocatecniStav, +MaxV1, +MaxV2, +KoncovyStav, -P):-P je posloupnost
% kroku, ktera prevadi PocatecniStav na KoncovyStav, MaxVi je objem nadoby i,
% posloupnost se hleda pomoci postupnehoprohlubovani (iterative deepening)
vyresID(PS, MaxV1, MaxV2, KS, P):-vyresID(PS, MaxV1, MaxV2, 1, KS, P).
vyresID(PS, MaxV1, MaxV2, N, KS, P):-nkroku(PS, MaxV1, MaxV2, N, P, KS),!.
vyresID(PS, MaxV1, MaxV2, N, KS, P):-N1 is N + 1, N1 < 50, % max pocet kroku je 50
                                    vyresID(PS, MaxV1, MaxV2, N1, KS, P).

% Vyhodou iterovaneho prohlubovani je, ze ma mensi prostorovou slozitost nez 
% prohledavani do sirky (nemusi si pamatovat vsechny navstivene stavy, staci mu 
% aktualni vetev) a navic zajistuje nalezeni nejkratsiho reseni (coz DFS se 
% stejnou prostorovou slozitosti nezarucuje).

% Napsat proste DFS je v Prologu samozrejme taky mozne a dokonce je to o neco 
% jednodussi nez postupne prohledavani.


% vyresDFS(+PocatecniStav, +MaxV1, +MaxV2, +KoncovyStav, -P):-stejne jako 
% predchozi, jen pouziva DFS misto ID
vyresDFS(PS, MaxV1, MaxV2, KS, P):-vyresDFS(PS, MaxV1, MaxV2, KS, [PS], P).
vyresDFS(_, _, _, KS, [KS|A], P):-reverse([KS|A], P), !.
vyresDFS(PS, MaxV1, MaxV2, KS, A, P):-akce(PS, MaxV1, MaxV2, NS), 
                                      \+member(NS, A), % jinak se zacyklime
                                      vyresDFS(NS, MaxV1, MaxV2, KS, [NS|A], P).