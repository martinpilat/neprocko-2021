% Author: Martin Pilat
% Date: 20. 3. 2017
% Update: 17. 3. 2020

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			5. cviceni -- Stromy
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% V Prologu lze pracovat i se slozitejsimi datovymi strukturami. Napr. s 
% binarnimi stromy se pracuje tak, ze si vytvorime term `t(LevySyn, Uzel, 
% PravySyn)`, ktery reprezentuje uzel a jeho dva syny. Prazdny uzel muzeme
% reprezentovat jako `nil` (`nil` neni zadne zvlastni klicove slovo Prologu, je
% to jen nularni term, kdybychom pouzili treba `nic` bude to fungovat uplne
% stejne).

% Strom, ktery ma koren 1, a dva syny 2 a 3 se tedy da zapsat jako 
% `t(t(nil,2,nil),1,t(nil,3,nil))`. Kdyz leveho syna (2) nahradime podstromem 
% `t(t(nil, 4, nil),2, t(nil,5, nil))` zapiseme vysledek jako 
% `t(t(t(nil,4,nil),2,t(nil,5,nil)),1,t(nil,3,nil))`.

% Zkusme si v Prologu praci s binarnimi stromy. Nejjednodussi jsou asi pruchody
% stromem. Zacneme tedy s prevedenim stromu na seznam v inorder poradi.

% inorderList(+T,-S) :- prevadi strom T na seznam S v inorder poradi
inorderList(nil,[]).
inorderList(t(X,Y,Z), S):-inorderList(X,S1),inorderList(Z,S2),append(S1,[Y|S2],S).

% preorder(+T,-S) :- prevadi strom T na seznam S v preorder poradi
preorderList(nil,[]).
preorderList(t(X,Y,Z), [Y|S]):-preorderList(X,S1),preorderList(Z,S2),append(S1,S2,S).

% postorder(+T,-S) :- prevadi strom T na seznam S v postorder poradi
postorderList(nil,[]).
postorderList(t(X,Y,Z), S):-postorderList(X,S1),postorderList(Z,S2),append(S1,S2,S3),append(S3,[Y],S).

% Obcas se misto seznamu prvku ze stromu hodi dalsi prvek vracet pri stisknuti
% `;` (nebo pri selhani vypoctu a hledani dalsiho ohodnoceni).

inorder(t(L,_,_),X):-inorder(L,X).
inorder(t(_,X,_),X).
inorder(t(_,_,R),X):-inorder(R,X).

preorder(t(_,X,_),X).
preorder(t(L,_,_),X):-preorder(L,X).
preorder(t(_,_,R),X):-preorder(R,X).

postorder(t(L,_,_),X):-postorder(L,X).
postorder(t(_,_,R),X):-postorder(R,X).
postorder(t(_,X,_),X).

% Samozrejme je mozne vytvorit i binarni vyhledavaci stromy. Pro to potrebujeme
% proceduru pro pridani do BST.

% pridejBST(+T,+X,+S) :- prida prvek X do stromu T a vrati strom S
pridejBST(nil, X, t(nil,X,nil)).
%pridejBST(t(L,X,P),X,t(L,X,P)).  % odkomentovani tehle radky zpusobi, ze se prvky nemohou opakovat
pridejBST(t(L,U,P),X,t(T,U,P)):-X=<U,pridejBST(L,X,T). % tohle je pak treba zmenit na X<U
pridejBST(t(L,U,P),X,t(L,U,T)):-X>U,pridejBST(P,X,T).

% Kdyz uz umime pridavat do BST, tak samozrejme umime i postavit BST ze seznamu
% (nize jsou dve mozne varianty).

% seznamNaBST(+S,-T) :- vytvori BST T ze seznamu S
seznamNaBST(S,T):-seznamNaBST(S,nil,T).
seznamNaBST([],T,T).
seznamNaBST([H|Hs],T1,T):-pridejBST(T1,H,T2),seznamNaBST(Hs,T2,T).

sB([], nil).
sB([X], t(nil,X,nil)).
sB([H|T], V):-sB(T,V1),pridejBST(H,V,V1).

% Pomoci prevedeni na seznam a inorder pruchodu lze samozrejme seznam i setridit:

% bstSort(+Seznam,-SetridenySeznam) :- tridi Seznam pomoci prevodu na BST a vraci
%									   SetridenySeznam
bstSort(S,T):-seznamNaBST(S,T1),inorderList(T1,T).

% __Ukol:__ Pokuste se nyni napsat prevod stromu na seznam tak, ze v nem budou
% prvky ulozeny po patrech. Tj. vlastne udelat pruchod stromem do sirky. Jak
% byste takovy program napsali? Hodi se vam na to rozdilove seznamy z minuleho
% cviceni.

% Stromy se ale daji pouzivat i jinak, nez jen pro BST. Muzeme jimi napriklad
% reprezentovat vyrazy. V uzlech jsou potom operatory v listech (uzlech, ktere
% obsahuji dva `nil`) jsou potom hodnoty. Zkusme si napsat vyhodnoceni takoveho
% stromu:

%vyhodnot(Strom, Hodnota):-vyhodnooti vyraz zadany jako Strom a vysledek vrati jako Hodnota
vyhodnot(t(nil, H, nil), H).
vyhodnot(t(LP, *, PP), H):-vyhodnot(LP, H1), vyhodnot(PP, H2), H is H1*H2.
vyhodnot(t(LP, +, PP), H):-vyhodnot(LP, H1), vyhodnot(PP, H2), H is H1+H2.
vyhodnot(t(LP, -, PP), H):-vyhodnot(LP, H1), vyhodnot(PP, H2), H is H1-H2.
vyhodnot(t(LP, /, PP), H):-vyhodnot(LP, H1), vyhodnot(PP, H2), H2 =\= 0, H is H1/H2.

% __Ukol:__ Umeli byste z vyrazu v postfixu takovy strom vyrobit? A co kdybyste
% dostali seznam a ukol mezi prvky seznamu pridat operatory a zavorky tak,
% aby vysledny vyraz mel zadanou hodnotu?