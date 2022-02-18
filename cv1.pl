% Author: Martin Pilat
% Date: 3. 2. 2014
% Last Updated: 19. 2. 2020

%%%%%%%%%%%%%%
% Prolog - uvod a program na rodinne vztahy
%%%%%%%%%%%%%%

% Program v prologu je v zasade databaze faktu a vztahu mezi nimi napsana pomoci 
% Hornovskych klauzuli. Kazda klauzule je napsana ve tvaru `H:-B`., kde `H` je 
% hlava klauzule a `B` je jeji telo. Muzete si to predstavovat i tak, ze `:-` 
% je sipka doleva (tedy implikace zprava doleva), a presne takovy je take 
% vyznam klauzule. Klauzule mohou mit i prazdne telo. Potom se pise jen `H`. 
% Napriklad nasledujici 4 klauzule rikaji, ze (postupne), `honza`, `adam`, 
% `david` a `jiri` jsou muzi.


muz(honza). 
muz(adam).
muz(david).
muz(jiri).

% Stejnym zpusobem dalsi klauzule rikaji, ze `jitka`, `jana` a `lida` jsou zeny.

zena(jitka).
zena(jana).
zena(lida).

% Klauzule mohou mit i vice nez jeden parametr. Nasledujici klauzule vyjadruje, 
% ze `honza` je potomek `adam`a. Podobne pro dalsi klauzule.

%potomek(X, Y)
%X je potomek Y
potomek(honza, adam).
potomek(adam, david).
potomek(jiri, adam).

potomek(jitka, adam).
potomek(jitka, jana).

% Pravidla, ktera plati mezi jednotlivymi klauzulemi, se daji napsat pomoci 
% klauzuli s telem. Napriklad fakt, ze kdyz je `X` potomek `Y`, a zaroven je 
% `X` zena, tak `X` je dcera `Y`. V logice bychom to mohli zapsat jako `zena(X) 
% & potomek(X,Y) -> dcera(X,Y)`. V Prologu napiseme to same, jen pomoci 
% prologovske syntaxe, tedy:

dcera(X,Y):-zena(X),potomek(X,Y).
syn(X,Y):-muz(X),potomek(X,Y).

% Vsimnete si, ze jmena konstant zacinaji malym pismenem a jmena promennych 
% velkym. Podobne jako jsme prave nadefinovali `dcera` a `syn`, muzeme definovat
% i dalsi rodinne vztahy.

deda(X,Y):-potomek(Y,Z),potomek(Z,X),muz(X).
bratr(X,Y):-potomek(X,Z),potomek(Y,Z),X \= Y,muz(X).

% K cemu je zapis `X\=Y` v definici `bratr`? `X\=Y` znamena, ze `X` a `Y` nejsou
% unifikovatelne. Bez tohoto atomu, bychom na dotaz `bratr(jiri, X)`, dostali i 
% odpoved, ze `jiri` je bratr sam sebe. A tim se dostavame k dotazum a k tomu, 
% co se Prologem da vlastne delat. 

% __Ukol__: Zkuste si pohrat s priklady v tomto textu, pridejte si dalsi lidi, 
% nadefinujte si napriklad predikaty `teta`, `babicka`, atd.

% Tento soubor si muzete i stahnout jako platny kod v Prologu. Pustte si 
% interpret Prologu, napr. [SWI-Prolog](http://www.swi-prolog.org/) a zkuste 
% tento soubor nacist (`consult(cv1).` -- musite byt ve spravnem adresari, pokud
%  v nem nejste, pouzijte bud `chdir('cesta/s/lomitky/dopredu')`, nebo 
% `working_directory(_,'cesta/s/lomitky/dopredu/')`. Funguje i zkratka `[cv1].` 
% a pokud mate priponu `.pl` asociovanou s Prologem, staci na soubor normalne \
% kliknout.

% Nyni uz muzete pokladat dotazy. 

% Dotazy se pisi primo v interpretu prologu, za prompt `?-` (odpovedi Prologu 
% jsou potom na samostatnych radkach). Nejjednodussi dotaz je napriklad:

%    ?- muz(jiri). 
%    true. 

% Prolog odpovida `true`, tzn. ze byl schopen najit v databazi (nebo z ni 
% odvodit), ze dotaz je pravdivy. Podobne, kdyz polozite dotaz:

%    ?- dcera(jitka, jana).
%    true.

% Prolog zase odpovi `true`, protoze byl z databaze schopen odvodit, ze 
% `jitka` je dcera `jany`.

% Muzete ale pokladat i slozitejsi dotazy, ktere obsahuji promenne, ty v Prologu
% musi zacinat velkym pismenem:

%    ?- dcera(X, jana). 
%    X = jitka ;
%    false.

% V tomto pripade vam Prolog odpovi, ze `X` je Jana, a ceka, co budete delat. 
% Kdyz zmacknete strednik, pokusi se najit dalsi `X`, ktere odpovida dotazu, 
% kdyz se mu to nepovede, odpovi `false`.

% Dotaz nemusi obsahovat vubec zadne konstanty. V takovem pripade Prolog najde 
% vsechna mozna ohodnoceni promennych v dotazu:

%    ?- syn(X,Y).
%    X = honza,
%    Y = adam ;
%    X = adam,
%    Y = david ;
%    X = jiri,
%    Y = adam.