% Author: Martin Pilat
% Date: 3. 2. 2014
% Updated: 28. 2. 2019

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Prolog - numeraly a seznamy
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Dnes si zkusime napsat trochu slozitejsi programy v Prologu, na kterych si 
% muzeme vyzkouset ruzne slozitejsi veci, napriklad si ukazat obousmernost 
% vypoctu. Poslouzi nam k tomu Peanova aritmetika. V te mame jedinou konstantu 
% `0`, a jedinou funkci `S(x)`, ktera vraci naslednika `x`. Daji se v ni potom 
% nadefinovat numeraly (cisla) tak, ze numeral `n` je `n`-krat aplikovana 
% operace `S` na `0`.

% Napisme napred proceduru pro soucet dvou numeralu.

% soucet(X,Y,Z)
% implementuje scitani numeralu, Z = X + Y
% prvni klauzule: 0+Y=Y
% druha klauzule: s(X)+Y=s(X+Y)
soucet(0,Y,Y).
soucet(s(X), Y, s(Z1)):-soucet(X,Y,Z1).

% Pohrajte si s touto procedurou. Co se stane, kdyz date na ruzna mista 
% promennou misto numeralu? Co treba dela `soucet(X, s(0), s(s(0)))`? A co \
% `soucet(X,Y,s(s(s(0))))`?

% Napiste predikat, ktery vytvori dvojnasobek zadaneho numeralu, a predikat, 
% ktery rozhodne jestli dany numeral reprezentuje sude cislo.

dvakrat(X,Y):-soucet(X,X,Y).
sude(X):-soucet(Z,Z,X).

% Zkuste napsat dalsi predikaty pro praci s numeraly -- soucin a mocninu. 
% Napiste predikat, ktery rozhodne, jestli zadany term je cislo.

% soucin(X,Y,Z) -- spocita XY a vrati vysledek v Z
soucin(_,0,0).
soucin(0,_,0).
soucin(X, s(Y), Z2):-soucin(X,Y,Z1),soucet(Z1,X,Z2).

% Umi predchazejici procedura `soucin/3`, rozkladat cisla? Co se stane, 
% kdyz zavolate `soucin(X,Y,s(s(s(s(s(s(0)))))))`. Proc? Upravte `soucin/3` tak, aby  v tomhle pripade fungoval.
    
% lt(X,Y) -- vrati true, kdyz X < Y
lt(X,s(X)).
lt(X,s(Y)):-lt(X,Y).

% cislo(X) -- rozhoduje, zda X je cislo.
cislo(s(X)):-cislo(X).
cislo(0).

soucin1(_, 0, 0).
soucin1(0, _, 0).
soucin1(s(X), Y, Z):-soucet(A, Y, Z), soucin1(X, Y, A).

% Jak byste v prologu reprezentovali datovou strukturu? Co treba struktura pro 
% datum?

% Prolog umi pracovat take se seznamy, ale na rozdil od jinych jazyku neumi 
% pristupovat rychle na dane misto v seznamu. Pristup trva linearne dlouho.

% Seznamy se pisou do hranatych zavorek, jejich prvky se oddeluji carkou napr. 
% `[1,2,3]` je seznam cisel `1`, `2` a `3`; `[1,2,[3,4]]` je seznam, ktery 
% obsahuje cisla `1`,`2` a seznam `[3,4]`. Prazdny seznam se zapise jako `[]`.
% Seznam se da rozdelit na prvni prvek a zbytek pomoci `[X|Xs]`, `X` potom 
% obsahuje prvni prvek seznamu a `Xs` zbytek. Podobne je mozne ziskat i prvni 
% dva prvky seznamu a zbytek jako `[X1, X2 | Xs]`. Kratsi nez dvouprvkovy 
% seznam se s timto termem neunifikuje.

% Uzitecnou procedurou pro praci se seznamy je `prvek(X,S)`, ktera rika, 
% jestli `X` je prvek seznamu `S`. Ve vetsine verzi tento predikat najdete 
% jako `member/2`. 

% prvek(?X, +S)
prvek(X,[X|_]).
prvek(X,[_|S]):-prvek(X,S).

% Procedura se podiva, jestli je prvni prvek seznamu unifikovatelny s `X`, 
% pokud ano, uspeje. Druha radka potom resi situaci, kdy `X` neni 
% unifikovatelne s prvnim prvkem seznamu, v takovem pripade se procedura
%  vola rekurzivne na zbytek seznamu.

% Muzeme napsat i proceduru, ktera pripoji prvek na zacatek seznamu. Je to
% hodne jednoduche, staci vysledny seznam unifikovat se seznamem, ktery ma na 
% prvnim miste `X` a zbytek je `S`.

%pridejz(+X, +S, -Z). Pridava X na zacatek seznamu S a vysledek vraci v Z
pridejz(X, S, [X|S]).

% Jak je to s pridanim prvku v seznamu na konec? Jak by takova procedura
% vypadala? A jaka bude jeji slozitost? Je trivialni pridat prvek na konec
% prazdneho seznamu, staci vytvorit jednoprvkovy seznam s timto prvkem. Pro
% pridani na konec delsiho seznamu vezmeme jeho prvni prvek, zkopirujeme ho do
% vysledneho seznamu a zavolame proceduru rekurzivne.

% pridejk(+X, +S, -Z). Pridava X na konec seznamu S a vysledek vraci v Z.
pridejk(X,[],[X]).
pridejk(X,[Y|Ys],[Y|Z]):-pridejk(X,Ys,Z).

% Vsimnete si, ze na zacatek seznamu umime pridavat v konstantnim case, kdezto
% na konec seznamu v case linearnim v delce seznamu.

% U obou predchozich predikatu jste si mohli vsimnout znaku `+/-` v komentari.
% Tyto znaky se pouzivaji pro rozliseni vstupnich a vystupnich promennych --
% `'+'` oznacuje vstupni promennou, `'-'` vystupni. Pri volani se potom
% ocekava, ze vstupni promenne maji konkretni hodnotu a vystupni jsou volne.
% Muzete se setkat i se znakem `'?'`, ten znamena, ze promenna muze byt vstupni
% i vystupni. Toto je dulezite pri cteni a psani dokumentace - pokud zavolate
% predikat jinak, nez je popsano, muze se zacyklit, nebo vracet nespravne
% vysledky. (Cely tento system notace je mnohem slozitejsi, muzete se
% [podivat do dokumentace][1], nam bude stacit zjednodusena verze popsana vyse.)

% Zkusme si nyni napsat otoceni seznamu pozpatku.

%otoc(+S,-Z). Otoci seznam S a vrati vysledek v Z
otoc([],[]). % otoceni prazdeho senzamu je prazdny seznam
otoc([X|Xs],Y):-otoc(Xs,Z),pridejk(X,Z,Y). % kdyz chces otocit [H|T] otoc T
                                           % a pripoj za to H

% Takhle napsane otoceni seznamu je neefektivni, trva kvadraticky dlouhou dobu
% ($n$-krat se vola `pridejk` na seznamy delky $1 \dots n$). Otoceni ale i v
% Prologu lze napsat v linearnim case, staci vyuzit jednoduchou techniku
% akumulatoru. Akumulator je promenna, do ktere si postupne ukladame
% mezivysledky a (typicky) v poslednim kroku rekurze ji prekopirujeme do
% vysledku. 

% S pomoci akumulatoru muzeme napsat proceduru `otoc2/2`, ktera seznam otoci v
% linearnim case. Pouzije k tomu pomocnou proceduru `otoc/3`, ktera navic
% pouziva akumulator. Ten je na zacatku prazdny.

%otoc2(+S,-Z). Otoci seznam S a vrati vysledek v Z.
otoc2(S,Z):-otoc2(S,[],Z).

%otoc2(+S, A, -Z). Otoci seznam S a vrati vysledek v Z, pouziva A jako akumulator.
otoc2([], A, A). % Seznam uz je prazdny, prekopiruj akumulator 
otoc2([X|Xs], A, Z):-otoc2(Xs, [X|A], Z). 	% Prvni prvek z [X|Xs] pripoj na zacatek
                                            % akumulatoru a rekurzivne se zavolej.

% Co se deje v tomto pripade? Misto, abychom pridavali prvky na konec
% vysledneho seznamu, pridavame je na zacatek akumulatoru. Kdyz se vstupni
% seznam vyprazdni, zkopirujeme akumulator do vysledku. Jak tedy probiha
% vypocet pro dotaz `otoc2([1,2,3,4], Z)`? Napred se zavola pomocna procedura
% `otoc2([1,2,3,4],[],Z)` a potom postupne `otoc2([2,3,4],[1],Z)`,
% `otoc2([3,4],[2,1],Z)`, `otoc2([4],[3,2,1],Z)` a `otoc2([], [4,3,2,1], Z)`.
% V tuto chvili se zavola prvni klauzule procedury `otoc2/3` a `Z` se unifikuje
% s akumulatorem `[4,3,2,1]`. Potom uz se program jen vrati s unifikaci
% `Z = [4,3,2,1]`.  Vsimnete si, ze v tomto pripade program bezel v dobe, ktera
% je linearni v delce seznamu, a ze jsme toho dosahli tim, ze jsme pridavani
% na konec seznamu nahradili pridavanim na zacatek akumulatoru, ktery jsme
% nakonec prekopirovali.

% Promyslete si dalsi operace pro praci se seznamy: `sudy`, `lichy` (rikaji,
% jestli ma seznam sudou nebo lichou delku), `prefix` a `sufix` (vraci postupne
% prefixy, nebo sufixy seznamu) atd.

sudy([]).
sudy([_,_|S]):-sudy(S).

lichy([_|S]):-sudy(S).

prefix([],_).
prefix([X|Xs], [X|Ys]):-prefix(Xs,Ys).

sufix(X,Y):-otoc2(Y,Y1),prefix(X1,Y1),otoc2(X1,X).

% [1]: https://www.swi-prolog.org/pldoc/man?section=modes