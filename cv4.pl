% Author: Martin Pilat
% Date: 13. 3. 2017
% Last update: 12. 3. 2020

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			4. cviceni -- Prohledavani, rozdilove seznamy, stromy
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Vetsina programu, ktere jsme zatim v Prologu napsali, se chovala 
% deterministicky, tj. v kazdem kroku byla jen jedna (rozumna) moznost, 
% co udelat, a tedy jsme se snazili, aby Prolog prave tuto variantu zvolil.
% V Prologu je ale mnohem beznejsi pouzivat prohledavani a programy jsou tedy 
% casto nedeterministicke - Prolog zkousi ruzne klauzule z definice procedury 
% dokud nenajde spravne reseni.

% ## Prohledavani

% Jako priklad nedeterministickeho programu muzeme vyresit znamy problem 
% loupezniku - vybrat ze seznamu cisla, jejichz soucet se rovna zadanemu cislu.
% Jedno z reseni muze vypadat napriklad nasledujicim zpusobem.

% vyber(S, N, Z) :- vybere ze seznamu S seznam Z jehoz soucet je presne N
vyber([], N, _):-N>0, fail.
vyber([], 0, []).
vyber([H|T], N, [H|Z]):-N1 is N-H,vyber(T,N1,Z).
vyber([_|T],N,Z):-vyber(T,N,Z).

% Posledni dve klauzule v tomto reseni rikaji, ze bud prvni cislo seznamu mame
% dat (druha radka od konce), nebo nemame (posledni radka). Prolog tedy napred
% zkusi do seznamu cislo dat, a vyresit novy problem s kratsim seznamem a mensim
% cislem, kdyz se to nepovede, selze a zkusi variantu, kde tam cislo neda.

% Program muzeme i zjednodusit. Posledni dve radky v sobe vlastne obsahuji 
% predikat `member`. Daji se napsat i pomoci nej, nebo pomoci predikatu 
% `select`, ktery funguje podobne, ale krome samotneho prvku ze seznamu vraci i
% seznam bez tohoto prvku. Druha varianta reseni tedy muze vypadat napriklad
% takto:

vyber2(X, N, Z):-vyber2(X, N, 0, [], Z).
vyber2(_, N, N, Z, Z).
vyber2(X, N, A, Z, V):-select(P, X, Xs), 
                       B is A + P, 
                       B =< N, 
                       vyber2(Xs, N, B, [P|Z], V).	

% ## Trideni

% Pro implementaci quicksortu se hodi procedura, ktera rozdeli seznam na dva,
% v jednom nich jsou cisla mensi nez zvolene  cislo, ve druhem vetsi.

% rozdel(+S, +Pivot, -Mensi, -Vetsi) :- rozdeli S tak, ze v seznamu Mensi jsou
% 	prvky mensi nebo rovny Pivotu a v seznamu vetai jsou prvky vetai nez Pivot

rozdel([], _, [], []).
rozdel([H|T], Pivot, [H|Mensi], Vetsi):-H=<Pivot,rozdel(T,Pivot,Mensi,Vetsi).
rozdel([H|T], Pivot, Mensi, [H|Vetsi]):-H>Pivot,rozdel(T,Pivot,Mensi,Vetsi).

% Abychom mohli napsat quicksort, potrebujeme jeste napsat spojeni dvou 
% seznamu. To by melo byt jednoduche. (V Prologu je spojeni take definovane 
% jako procedura `append`):

% spojeni(?X, ?Y, ?Z) :- do seznamu Z ulozi spojeni seznamu X a Y
spojeni([],X,X).
spojeni([H|T], X, [H|S]):-spojeni(T,X,S).

% A ted uz mame vsechno a muzeme klidne napsat quicksort. `T\=[]` v posledni
% klauzuli zajistuje, ze Prolog nebude vracet stejne odpovedi vicekrat, ale
% neni ho tam nutne mit (vsechny vracene odpovedi jsou spravne, dalo by se mu
% vyhnout i tak, ze bychom chteli, aby seznam mel aspon dva prvky `[H1, H2|T]`).

% qsort(+X,-Y) :- do seznamu Y ulozi setrideny seznam X
qsort([],[]).
qsort([A],[A]).
qsort([H|T],X):-T\=[],rozdel(T,H,Mensi,Vetsi),
                qsort(Mensi,X1),qsort(Vetsi,X2),
                spojeni(X1,[H|X2],X).

% ## Rozdilove seznamy

% Pri implementaci quicksortu jsme potrebovali zretezovat seznamy. Zretezeni
% seznamu trva dlouho (linearne v delce prvniho seznamu), v Prologu ale muzeme
% pouzit trik, kteremu se rika rozdilovy seznam. Spojeni rozdilovych seznamu
% potom jde napsat v konstantnim case.

% Jak ten trik funguje? Misto pouziti pouze samostatneho seznamu pouzijeme
% seznam a volnou promennou. Bezne se tyto dve casti oddeluji symbolem `-`
% (proto se jim asi rika rozdilove seznamy), muzete si je ale klidne oddelit
% i jinak, pripadne si nadefinovat vlastni datovou strukturu (binarni term).

% Rozdilovy seznam tedy ma dve casti, samotny seznam a volnou promennou, napr.
% `S-X`. Tohle ale samo o sobe nestaci, dulezite je, ze ta promenna `X` je
% zaroven pouzita jako konec seznamu `S`. Takovy rozdilovy seznam napriklad
% muze vypadat nasledujicim zpusobem `[a,b,c|X]-X`.

% Napsat prevod seznamu na rozdilovy seznam je snadne (v linearnim case).

% narozdil(+S,-RS) :- prevadi seznam S na rozdilovy seznam RS
narozdil([],X-X).
narozdil([H|T],[H|S]-X):-narozdil(T,S-X).

% Prevod z rozdiloveho seznamu na obycejny seznam je jeste jednodussi (a
% dokonce v konstantnim case).

% naobyc(+RS,S):- prevadi rozdilovy seznam RS na obycejny seznam S
naobyc(X-[],X).

% Kdyz nyni v rozdilovem seznamu `[a,b,c|X]-X` unifikujeme `X` s libovolnym
% seznamem (napr. `[d,e]`), dostaneme `[a,b,c,d,e]-[d,e]` (v jednom kroku) a
% vidite, ze mame spojeni seznamu (+ nejaky ocasek navic). Jeste zajimavejsi
% situace by nastala, kdybychom spojili nas puvodni seznam s rozdilovym
% seznamem, napr. s `[d,e|Y]-Y`, a dostali bychom `[a,b,c,d,e|Y]-Y` - tedy zase
% rozdilovy seznam. Takove spojeni se ale da opet napsat velmi snadno:

%spojeniRS(+A,+B,-C):-rozdilovy seznam C je spojeni rozdilovych seznamu A a B
spojeniRS(A-B,B-B1,A-B1).

% Posledni ukol pro dnesek (vicemene nesouvisejici s predchazejicim) je napsani
% transpozice matice. Matice je zadana jako seznam seznamu. Pro transpozici
% (tuhle implementaci) potrebujeme pomocnou proceduru,  ktera ze seznamu
% seznamu odebere prvni prvky a vrati zbytky.

%vsePrazdne(+SeznamSeznamu):-vsechny seznamy v SeznamSeznamu jsou prazdne
vsePrazdne([]).
vsePrazdne([[]|Z]):-vsePrazdne(Z).

%hlavyZbytky(+SeznamSeznamu, -Hlavy, -Zbytky):-rozdeli vsechny seznamu v SeznamSeznamu na Hlavy a Zbytky
hlavyZbytky([], [], []).
hlavyZbytky([[H|T] | Z], [H|PP], [T|ZB]):-hlavyZbytky(Z, PP, ZB).

%transpozice(+M, -TM):-TM je transpozice matice M
transpozice(M, []):-vsePrazdne(M).
transpozice(M, [H|TM]):-hlavyZbytky(M, H, Z), transpozice(Z, TM).