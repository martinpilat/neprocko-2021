-- Author: Martin Pilat
-- Date: 3. 4. 2017
-- Updated: 31. 3. 2020

----------------------------------------------------------------------------
--          1. cviceni -- Haskell - typy, zakladni funkce
----------------------------------------------------------------------------

-- Po cvicenich s Prologem se dnes presuneme k dalsimu neproceduralnimu jazyku. 
-- Tentokrat nepujde o logicke programovani, ale o programovani funkcionalni, 
-- ktere si ukazeme na jazyce Haskell.

-- Zakladni vlastnosti funkcionalniho programovani je, ze cely program se sklada
-- z funkci, jejichz vystupy zavisi pouze na jejich vstupech (a ne treba na 
-- stavu programu). Neexistuji zadne globalni promenne. Cely program je nakonec
-- jedna (mnohokrat slozena a pripadne i hodne slozita) funkce.

-- Dalsi vlastnosti funkcionalniho programovani je, ze se v nem casto pracuje s
-- funkcemi, ktere jako parametry maji dalsi funkce.

-- Podivejme se napred, jak v Haskellu pracovat se zakladnimi datovymi typy. 
-- Haskell defaultne pocita s dlouhymi cisly (`Integer`), takze kdyz na vstupu 
-- interpretu napisete `5^87`, on vam bez problemu odpovi
-- `6462348535570528709932880406796584793482907116413116455078125`.

-- Haskell samozrejme zna i dalsi obvykle datove typy: `Int` (pro bezna, 
-- "kratka" cisla), `Bool`, `Float`, `Double`, `Char`, ... (vsechny s obvyklym 
-- vyznamem). Haskell podporuje i usporadane `n`-tice, psane v kulatych 
-- zavorkach. `(Int, Double)` je dvojice, kde prvni prvek je `Int` a druhy 
-- `Double`. Konecne, Haskell take zna seznamy, ktere jste si jiste moc oblibili
-- v Prologu :). Seznam prvku typu `Integer` se zapise jako `[Integer]`, syntaxe
-- pro rozdeleni seznamu na hlavu a telo je `x:xs`. Retezce jsou v Haskellu 
-- seznamy znaku `[Char]`.

-- Zkusme si ted v Haskellu napsat prvni jednoduchou funkci, napriklad vypocet 
-- faktorialu, ukazeme si na ni zakladni syntaxi a take to, jak se k funkcim 
-- pisou typy.

fact :: Integer -> Integer  -- typ
fact n  | n < 1     = error "To teda ne"    -- pro zaporna cisla neni definovan
        | n == 1    = 1                     -- faktorial 0 je 1
        | otherwise = n * fact (n-1)        -- faktorial n je n*faktorial n-1

-- Prvni radka explicitne rika, jakeho typu nase funkce `fact` je. Ze zobrazuje 
-- `Integer` na `Integer`. V interpretu si muzeme jeji typ nechat vypsat pomoci
-- `:t fact` a dostaneme odpoved, ktera presne odpovida prvni radce. Typy neni
-- nutne uvadet, Haskell si je umi odvodit sam, ale je to vhodne, pomaha to pri 
-- ladeni a zaroven to pomuze komukoliv, kdo cte vas kod. Zaroven, zrovna v 
-- pripade faktorialu, si Haskell odvodi typ moc obecne a nechal by vas ho 
-- pocitat i pro necela cisla (ale stejne byste minuli ten konec rekurze a pak
-- by vypsal tu chybovou hlasku).

-- Syntaxi s `|` a `=` se rika straz. Mezi `|` a `=` muzete napsat libovolnou 
-- podminku, je to podobne tomu, kdyz v matematice definujete funkci po castech.

-- Zkusme si ted napsat nekolik zakladnich funkci pro praci se seznamy (vsechny
-- z nich jsou v Haskellu definovane, ale nam se pro procviceni hodi). Pro 
-- prehlednost (a zabraneni kolizim s existujicimi jmeny) budeme pred jejich 
-- standardni nazvy psat `my_`.

-- Prvni uzitecnou funkci je funkce `map`, ktera jako argument vezme funkci a 
-- seznam a aplikuje ji na kazdy prvek seznamu, vrati seznam s vysledky.

my_map :: (a -> b) -> [a] -> [b]
my_map f []     = []
my_map f (x:xs) = (f x):(my_map f xs)

-- Muzeme ji hned vyzkouset v interpretu.

-- >my_map (*2) [1,2,3,4]
-- [2,4,6,8]

-- Vsimnete si, ze funkce muze byt definovana i na vice radkach. Haskell pouziva 
-- expression matching a pouzije definici, ktera je jako prvni a odpovida 
-- zvolenemu tvaru vstupu. V tomto pripade se pro prazdny seznam zavola prvni 
-- radek a kdykoliv jindy radek druhy.

-- Hodi se i par vysvetleni k typu. Operator `(*)` je v Haskellu typu 
-- (zjednodusene) `Int -> Int -> Int`, tzn. ze na vstupu ocekava dve cisla a 
-- vraci cislo. Tim, ze napiseme `(*2)` jsme jedno cislo dodali, a tim dostavame 
-- funkci typu `Int -> Int` (tento zapis funguje pro vsechny operatory a binarni
-- funkce pokud je zapiseme v inline zapisu - ukazeme si pozdeji). Samotny zapis
-- typu funkce `my_map` rika, ze jako prvni argument dostava funkci typu 
-- `(a -> b)`, potom seznam prvku typu `a` a vraci seznam prvku typu `b`. 
-- Vsimnete si, ze obecne napsane typy se pisou malym pismenem.

-- A jeste jedna poznamka, pokud pisete `2*3`, staci psat `*`, pokud ale chcete 
-- pouzit operator `*` jako parametr funkce, musite psat `(*)`. Interpretu
-- muzete tedy napriklad napsat i `(*) 2 3` a on vam odpovi `6` (`* 2 3` ale 
-- nefunguje). Stejne pravidlo samozrejme plati i pro ostatni operatory.

-- Uzitecna funkce je i `take n xs`, ta vezme ze seznamu `xs` prvnich `n` prvku
-- a vrati je.

my_take :: Int -> [a] -> [a]
my_take 0 _                     = []
my_take _ []                    = []
my_take n (x:xs) | n < 0        = error "n must be > 0"
                 | otherwise    = x:(my_take (n-1) xs) 

-- Vsimnete si, ze opet muzeme pouzit `_` jako nazev promenne, ktera nas 
-- nezajima.

-- Vyzkousejme si nasi funkci:

-- > my_take 3 [1,2,3,4,5]
-- [1,2,3]

-- Haskell dovoluje definovat seznamy i o neco obecneji. Muzete definovat rozsah 
-- napriklad jako `[3..10]` je seznam vsech cisel od `3` do `10` (vcetne). 
-- Dokonce horni mez seznamu muze chybet, potom se jedna o nekonecny seznam. 
-- `[1..]` tedy definuje seznam `[1,2,3,4,5,6,7,...]`. Zajimave je, ze i s 
-- takovymi seznamy jde v Haskellu bez problemu pracovat.

-- > my_take 5 [1..]
-- [1,2,3,4,5]

-- V tomhle pripade se projevuje jedna z vlastnosti Haskellu. A tou je line 
-- vyhodnocovani, tzn. ze Haskell nevyhodnocuje vyrazy, dokud nemusi. A presne 
-- to se stalo s nasim nekonecnym seznamem, nikdy jsme nepotrebovali prvky od 6
-- dal, takze Haskell na ne nikdy ani nesahl. Dokonce muzeme zkombinovat nase 
-- dve funkce a napsat funkci, ktera nam vrati seznam prvnich `n` mocnin `2`. 
-- Opet, dalsi mocniny se nepocitaji, kdyz nejsou potreba.

mocninyDvojky :: Int -> [Integer]
mocninyDvojky n = my_take n (my_map (2^) [1..])

-- Dokonce konec teto funkce definuje seznam mocnin dvojek, kdyby se nam treba
-- nekdy hodil.

seznamMocnin = my_map (2^) [1..]

-- Uzitecna je take varianta funkce `take`, a sice `takeWhile`, ktera misto
-- poctu ma funkci, ktera vraci `Bool`. `takeWhile` potom bere prvky ze seznamu
-- dokud tato funkce vraci True.

my_takeWhile :: (a -> Bool)->[a]->[a]
my_takeWhile _ []                 = []
my_takeWhile f (x:xs) | f x       = x:(takeWhile f xs)
                      | otherwise = []

-- Dalsi uzitecnou standardni funkci je `zip`. Ta vytvari ze dvou seznamu jeden 
-- seznam dvojic. Delka vysledneho seznamu odpovida delce kratsiho z obou
-- seznamu.

my_zip :: [a] -> [b] -> [(a,b)] 
my_zip [] _          = []
my_zip _ []          = []
my_zip (x:xs) (y:ys) = (x,y):my_zip xs ys

-- > my_zip [1..5] seznamMocnin
-- [(1,2),(2,4),(3,8),(4,16),(5,32),(6,64),(7,128),(8,256),(9,512),(10,1024)]

-- `zipWith` je take uzitecna, vezme dva seznamy, aplikuje na ne binarni funkci
-- a vysledky ulozi do vysledneho seznamu.

my_zipWith :: (a->b->c) -> [a] -> [b] -> [c]
my_zipWith _ _ []          = []
my_zipWith _ [] _          = []
my_zipWith f (x:xs) (y:ys) = (f x y):my_zipWith f xs ys

-- Pomoci ni se da treba napsat scitani dvou seznamu po slozkach

sectiSeznamy x y = my_zipWith (+) x y

-- Ukazme jeste jednu standardni funkci, `foldl` aplikuje operaci na seznam tak,
-- ze ji vlozi mezi vsechny prvky seznamu, jeste je mozne specifikovat pocatecni
-- hodnotu. Napr. `foldl (+) 7 [1,2,3,4]` spocita `(((7 + 1) + 2) + 3) + 4`.
-- Jeji varianta `foldr` dela totez, ale prvky seznamu bere odzadu (tj. pocita
-- `1 + (2 + (3 + (4 + 7)))`).

my_foldl :: (a->b->a)->a->[b]->a
my_foldl _ init []      = init
my_foldl f init (x:xs)  = my_foldl f (f init x) xs

-- Jeste existuje varianta `foldl1`, ktera jako pocatecni hodnotu vezme prvni
-- prvek seznamu.

my_foldl1 :: (a->a->a)->[a]->a
my_foldl1 f (x:xs) = my_foldl f x xs

-- Ted muzeme treba naprogramovat skalarni soucin dvou seznamu.

skalarniSoucin :: Num a => [a] -> [a] -> a
skalarniSoucin x y = my_foldl1 (+) (my_zipWith (*) x y)

-- Specifikace `Num a` v typu funkce rika, ze `a` musi byt nejaky numericky typ.
-- Jedna se o tzv. typovou tridu, jen si dejte pozor na to, ze typova trida neni
-- sama o sobe typ. 

-- Zkusme jeste neco trochu slozitejsiho (alespon ve smyslu typu vysledne
-- funkce). Funkce `filter` ma jako jeden z parametru funkci, ktera vraci
-- `Bool`, a jako druhy parametr seznam. Vybere ze seznamu prvky, pro ktere je
-- dana funkce `True`.

my_filter :: (a->Bool)->[a]->[a]
my_filter _ []              = []
my_filter f (x:xs)  | f x       = x:(filter f xs)
                    | otherwise = filter f xs

-- Opet mame k dispozici par jednoduchych funkci, ktere `Bool` vraceji, napr. 
-- porovnani `(<,>,<=,>=, ==, /=)`. Kdyz jim opet jeden parametr "obsadime",
-- dostavame funkci, ktera ma jeden parametr a vraci `Bool`. Muzeme tedy napr.
-- ze seznamu cisel vyfiltrovat ta, ktera jsou mensi nez 5.

-- > my_filter (<5) [1,2,3,4,5,43,2,3,4]
-- [1,2,3,4,2,3,4]

-- Pomoci teto funkce ale muzeme udelat mnohem zajimavejsi veci, napr. napsat 
-- quicksort.

qsort :: Ord a => [a] -> [a]
qsort []        = []
qsort (x:xs)    = (qsort mensi)++[x]++(qsort vetsi)
                where 
                    mensi = my_filter (<x) xs
                    vetsi = my_filter (>=x) xs

-- Konstrukce `where` dovoluje definovat lokalne funkce primo uvnitr jine
-- funkce, muze obcas trochu zprehlednit kod (kdybychom ale misto
-- `mensi`/`vetsi` napsali primo tu jejich definici, dostali bychom stejny
-- vysledek). Operator `(++)` spojuje dva seznamy (v case linearnim v delce
-- prvniho z nich). 

-- Haskell pouziva (relativne volnou) 2D syntaxi, tj. zalezi na odsazeni.
-- Podstatne ale je jen za klicovymi slovy `where`, `let`, `do` a `of`. Pravidlo
-- pritom je takove, ze hlubsi odsazeni za temito slovy vytvari novy blok,
-- stejne odsazeni pokracuje v existujicim bloku a mensi odsazeni blok ukoncuje.
-- Pokud by nekoho zajimaly detaily, daji se najit v 
-- [dokumentaci](https://www.haskell.org/onlinereport/lexemes.html#lexemes-layout). 
-- Misto odsazeni lze pouzivat i slozene zavorky a stredniky, v Haskellu to ale 
-- neni moc obvykle.