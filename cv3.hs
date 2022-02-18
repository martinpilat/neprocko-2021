-- Author: Martin Pilat
-- Date: 24. 4. 2017
-- Updated: 21. 4. 2020

------------------------------------------------------------------------------
--          10. cviceni -- vlastni typy, stromy
------------------------------------------------------------------------------

-- Uzitecnou vlastnosti Haskellu jsou list comprehensions. V jednoduchych 
-- pripadech se chovaji jako kombinace `map` a `filter`. Umoznuji nam ze seznamu
-- vybrat prvky, ktere splnuji nejakou podminku a rovnou na ne aplikovat nejakou
-- funkci. Syntaxe je podobna matematicke syntaxi pro vyber prvku z mnoziny.
-- Napr. pokud chceme ze seznamu `xs` vybrat prvky mensi nez zvolene cislo, 
-- muzeme to napsat jako 

mensiNez :: Ord a => a -> [a] -> [a]
mensiNez n xs = [x | x <- xs, x < n]

-- Pokud zaroven tyto prvky budeme chtit vynasobit dvema, muzeme to napsat 
-- takhle:

mensiNez2 :: (Ord a, Num a) => a -> [a] -> [a]
mensiNez2 n xs = [2*x | x <- xs, x < n]

-- Samozrejme s list comprehension jde delat i slozitejsi veci. Jeden uzitecny
-- priklad je napr. seznam vsech usporadanych dvojic. Ten uz se jen pomoci 
-- `map` a `filter` napsat neda. Jak presne tato konstrukce funguje si povime
-- pozdeji, az si budeme povidat o monadach.

usporadaneDvojice :: [a] -> [b] -> [(a,b)]
usporadaneDvojice xs ys = [(x,y) | x <- xs, y <- ys]

-- A co treba jen dvojice cisel, jejichz soucet je vetsi nez zvolene `n`?

usporadaneDvojiceVetsi :: (Ord a, Num a) => [a] -> [a] -> a -> [(a,a)]
usporadaneDvojiceVetsi xs ys n = [(x,y) | x <- xs, y <- ys, x + y > n]

-- Samozrejme i v Haskellu si muzete nadefinovat svoje vlastni typy. Cely 
-- typovy system Haskellu je univerzalnejsi, nez typove systemy, na ktere jste
-- asi zvykli z jinych programovacich jazyku. 

-- Nejjednodussi variantou definice noveho typu je to, co se v jinych jazycich 
-- nazyva enum. Proste seznam konstant. Prikladem takove definice muze byt napr.
-- seznam ruznych barev, ktere chcete nekde pouzit.

data Barva = Cervena | Zelena | Modra | Bila | Cerna deriving (Show)

-- Novy typ se definuje pomoci klicoveho slova `data`, za kterym nasleduje nazev
-- typu (presneji typovy konstruktor), potom `=` a seznam datovych konstruktoru
-- oddelenych symbolem `|`. V pripade typu `Barva` tedy `Barva` je nazev typu
-- (typovy konstruktor) a seznam `Cervena | Zelena | ...` jsou datove
-- konstruktory. Proc se tomu rika konstruktor si povime za chvili. Prozatim si
-- muzete predstavovat, ze `Cervena,  Zelena`, atd. jsou mozne hodnoty tohoto
-- typu. Dulezite je, ze jak typove tak datove konstruktory musi zacinat velkym
-- pismenem. Posledni cast definice noveho typu (`deriving (Show)`) rika, ze typ
-- patri do typove tridy `Show`,  tzn. ze je mozne jeho hodnoty vypisovat, bez
-- toho by je vypsat neslo. Haskell si sam vytvori jednoduchou funkci, ktera
-- typ zobrazuje, neni treba ji psat.

-- Kdyz uz mame nadefinovany typ barva, muzeme napriklad napsat funkci, ktera
-- prevadi barvu na trojici RBG hodnot.

barvaNaRGB :: Barva -> (Int, Int, Int)
barvaNaRGB Cervena  = (255,0,0)
barvaNaRGB Zelena   = (0,255,0)
barvaNaRGB Modra    = (0,0,255)
barvaNaRGB Bila     = (255,255,255)
barvaNaRGB Cerna    = (0,0,0)

-- Muzete si vsimnout, ze i na nove definovany typ funguje pattern matching.

-- Mit typ, ve kterem jde pouze reprezentovat par konstant, ale pro reprezentaci
-- vsech barev nestaci. Hodilo by se nam, krome moznosti mit barvu jako
-- konstantu mit i moznost zadefinovat ji pomoci rgb slozek. K tomu se hodi
-- dalsi typ. (Jmena barev konci na RGB protoze mohou byt definovana jen
-- jednou.)

data BarvaRGB = CervenaRGB | ModraRGB | ZelenaRGB | BilaRGB | CernaRGB | RGB Int Int Int deriving (Show)

-- Vsimneme si, ze pro reprezentaci trojice se pouzije jiny datovy konstruktor,
--  v tomhle pripade se jmenuje `RGB` a jako parametry ma 3 krat `Int`.

-- Jaky typ ma RGB? 


-- > :t RGB
-- RGB :: Int -> Int -> Int -> BarvaRGB


-- Datove konstruktory jsou tedy z pohledu Haskellu funkce, a to take
-- vysvetluje,  proc se jim rika konstruktory - vytvari z ruznych hodnot hodnotu
-- typu `BarvaRGB`.

-- Napisme ted funkci, ktera trojici RGB prevede na barvu, pouzije konstantu,
-- pokud ji pro takovou kombinaci mame, jinak pouzije RGB trojici.

rgbNaBarvu :: (Int, Int, Int) -> BarvaRGB
rgbNaBarvu (0,0,0)          = CernaRGB
rgbNaBarvu (255,255,255)    = BilaRGB
rgbNaBarvu (0,0,255)        = ModraRGB
rgbNaBarvu (0,255,0)        = ZelenaRGB
rgbNaBarvu (255,0,0)        = CervenaRGB
rgbNaBarvu (a,b,c)          = RGB a b c 

-- Vsimnete si univerzalnosti typoveho systemu Haskellu. V typu `BarvaRGB` muze
-- byt ulozena jak konstanta, tak trojice cisel. A to jeste zdaleka neni
-- vsechno :).

-- Muzeme napriklad nadefinovat typ `Tvar`, do ktereho budeme ukladat bud
-- ctverec jako souradnice leveho horniho rohu a delky strany, nebo kruh, jako
-- souradnice stredu a polomer.

data Tvar = Ctverec Int Int Int | Kruh Int Int Int

-- Podobny typ by se dal v C/C++ napsat jako union, v Jave a ostatnich vyssich 
-- uz potrebujeme dedicnost. Nebo si nekam poznamenat, jestli ta trojice
-- reprezentuje kruh nebo ctverec. V Haskellu nic z toho delat nemusime, funkce,
-- ktera by pracovala s timto typem muze udelat pattern matching a rozhodnout
-- se podle toho, jestli chce zrovna `Kruh`, nebo `Ctverec`.

-- Ale ani tady flexibilita typoveho systemu v Haskellu nekonci. Kdo Haskell
-- podcenil a myslel si, ze nezvladne genericke typy, tak se myli :). 

-- Zacneme pro jednoduchost typem, ktery nam umozni ukladat dvojice prvku
-- stejneho typu (narozil od `(a,b)`, kde `a` a `b` muzou mit ruzny typ).

data Pair a = P a a

-- Tady je ted videt, proc se `Pair a` rika typovy konstruktor. Kdyz za `a`
-- dosadime napr. `Int` dostaneme teprve typ - `Pair Int`, do ktereho si muzeme
-- ulozit libovolne dva prvky typu `Int`.

-- V prikladu nahore jsme pouzili ruzne nazvy pro typovy a datovy konstruktor,
-- ale neni to vubec nutne. Mohli jsme klidne napsat neco jako


data Dvojice a = Dvojice a a deriving (Show)

-- V tomto pripade se oba konstruktory jmenuji stejne. Je to obvykly zpusob
-- pojmenovani konstruktoru.

-- Napisme ted funkce `prvni` a `druhy`, ktere vraci prvni a druhy prvek dvojice.

prvni::Dvojice a -> a
prvni (Dvojice a _) = a

druhy::Dvojice a -> a
druhy (Dvojice _ a) = a

-- Mozna pro vas je pojmenovani obou konstruktoru stejne trochu matouci, ale
-- zvyknete si. Dulezite je si uvedomit, ze typove konstruktory se objevuji
-- jen v definici typu funkce. V samotne funkci uz se objevuji jen datove
-- konstruktory.

-- Vyzkousejte si prave nadefinovanou funkci a potom napiste funkci, ktera
-- prevadi nasi dvojici na normalni Haskellovsky par `(x,y)`.

naTuple :: Dvojice a -> (a,a)
naTuple (Dvojice a b) = (a,b)