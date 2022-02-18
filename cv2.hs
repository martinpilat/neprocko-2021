-- Author: Martin Pilat
-- Date: 17. 4. 2014
-- Updated: 14. 4. 2020

----------------------------------------------------------------------------
--          10. cviceni -- Haskell - seznamy, skladani funkci, scan
----------------------------------------------------------------------------

-- Jako dobry zdroj jednoduchych (a cim dal slozitejsich) problemu pro zkouseni
-- si Haskellu nam muze poslouzit [Project Euler](http://projecteuler.net). 
-- Napr. problem c. 6 po nas chce, abychom spocitali rozdil mezi souctem ctvercu
-- a ctvercem souctu prvnich 100 prirozenych cisel. Udelejme to obecne pro 
-- prvnich `n`.

problem6 :: Int -> Int
problem6 n = (sum [1..n]) ^ 2 - (sum (map (^2) [1..n]))

-- Definujme si ted seznam vsech prvocisel (protoze v Haskellu proste muzeme :)).
-- Nejdrive se nam hodi funkce, ktera rozhoduje, jestli dane cislo je prvocislo.
-- Implementace tady neni moc efektivni, proste zkousi vsechna cisla mensi nez 
-- odmocnina z testovaneho a zjistuje, jestli je jimi dane cislo delitelne. Ceho
-- si ale muzete vsimnout je, ze kdyz budete testovat nejake hodne velke sude 
-- cislo algoritmus skonci skoro hned. Staci totiz zkusit delit dvojkou a 
-- vysledek uz je jasny, dalsi se nezkousi. Tady je dulezite pouziti funkce 
-- `and`, kdybyste pouzili (na prvni pohled ekvivalentni) `foldl1 (&&)`, tak se 
-- bude zkouset delit vsemi cisly, `foldl` totiz nevi, ze `&&` s `False` bude uz
-- vzdycky `False`.

-- Syntax `\x -> ...` definuje anonymni funkci parametru `x`. Prave v pripade 
-- ruznych funkci vyssich radu (`map`, `foldl`, ...) se casto pouzivaji.

prime :: Integer -> Bool
prime n | n < 2     = False --cisla mensi nez 2 nejsou prvocisla
        | otherwise = and (map (\x -> (n `mod` x /= 0)) (takeWhile (\x -> x*x <= n) [2..n]))

-- A ted uz muzeme nadefinovat seznam vsech prvocisel.

primes :: [Integer]
primes = filter prime [1..]

-- Prvnich `n` prvocisel pak dostaneme snadno pomoci `take`.

prvniPrvocisla :: Int -> [Integer]
prvniPrvocisla n = take n primes

-- A prvocisla mensi nez nejake cislo `n` pomoci `takeWhile`

prvocislaMensi :: Integer -> [Integer]
prvocislaMensi n = takeWhile (<n) primes

-- Minule jsme si ukazali (a naprogramovali) zakladni funkce pro praci se 
-- seznamy v Haskellu. Ukazali jsme si na nich zaklady Haskellovske syntaxe. 
-- Dneska se podivame na dalsi takove funkce a ukazeme si, jak v Haskellu 
-- efektivne s temito funkcemi pracovat. 

-- V Haskellu je bezne, ze na vetsinu prace se seznamy nepotrebujeme nijak 
-- rekurzivne volat svoje funkce. Vsechny zakladni iterace uz jsou totiz 
-- napsane. To same plati pro vetsinu ostatnich datovych typu. Haskell se snazi 
-- konstrukce, ktera se casto opakuji napsat obecne. 

-- Z tohoto duvodu muzeme treba uplne jednoduse napsat Hornerovo schema. Staci
-- si uvedomit, co se v nem presne pocita. Vlastne vzdycky chceme vzit predchozi
-- vysledek, vynasobit ho 10 a pricist k nemu dalsi cislo. To je presne to, co 
-- dela funkce `foldl`.

-- Funkci, kterou je potreba dat `foldl` jako parametr `(10*x + y)` nemusime 
-- definovat zvlast. Muzeme vyuzit anonymni funkce. Syntaxe je jednoducha, 
-- napise se `\`, za nej parametry, potom `->` a za ni, co ma funkce pocitat. 
-- V pripade funkce, ktera z cisel `x` a `y` pocita `10*x + y`, tedy anonymni 
-- zapis vypada jako `\x y -> 10*x + y`.

horner :: Num a => [a] -> a
horner = foldl (\x y -> 10*x + y) 0

-- Krome `foldl`, ktera je zleva asociativni, jeste existuje verze `foldr`, 
-- ktera je naopak asociativni zprava. Samotna funkce `foldr` je uzitecna a da 
-- se snadno predstavit, co dela. Staci si uvedomit, ze seznam `[1,2,3,4,5]` se
-- v Haskellu da definovat pomoci `(:)` jako `1:2:3:4:5:[]`. Funkce `foldr` dela
-- to, ze `(:)` nahradi libovolnou funkci, a `[]` nahradi nejakou pocatecni 
-- hodnotou. Napr. `foldr (:) [] [1,2,3,4,5]` vrati presne seznam `[1,2,3,4,5]`.
-- Pokud `(:)` nahradime za `(+)` a `[]` za `0`, dostaneme presne to, co dela
-- `foldr (+) 0 [1,2,3,4,5]`. A to je `1+(2+(3+(4+(5+0))))`. (Naproti tomu 
-- `foldl (+) 0 [1,2,3,4,5]` pocita `(((((0+1)+2)+3)+4)+5)`.)

-- `foldr` je dokonce tak obecna, ze pomoci ni jde napsat libovolnou primitivne
-- rekurzivni funkci (tj. cokoliv, co umite naprogramovat pomoci for cyklu, ale
-- napr. bez while cyklu).

-- Pro ukazku si muzeme zkusit napsat treba `map` pomoci `foldr`.

foldrMap :: (a -> b) -> [a] -> [b]
foldrMap f = foldr (\x y -> (f x):y) []

-- Dokonce se pomoci `foldr` da napsat i `foldl` (spis pro zajimavost, zamyslete
--  se, jak to vlastne funguje a nepouzivejte to, je to hodne skladani funkci).

foldrFoldl f a bs = foldr (\b g x -> g (f x b)) id bs a 

-- `foldr` ma oproti `foldl` jeste jednu vyhodu. Za urcitych okolnosti muze 
-- `foldr` fungovat i na nekonecnych seznamech, to `foldl` nikdy neumi 
-- (potrebuje vzdycky dojit az na konec seznamu, nez zacne pocitat).

-- Takze napr. `foldr (&&) False (repeat False)` skonci, ale 
-- `foldl (&&) False (repeat False)` se zacykli. Rozdil je v tom, jak je 
-- definovane `(&&)`, to rika, ze kdyz je prvni argument False, ma vratit
-- False, jinak vraci druhy argument. Pri pouziti `foldr` se tedy staci podivat
-- na prvni argument a je rozhodnuto, zbytek seznamu neni treba vyhodnocovat.
-- U `foldl` to tak nefunguje, ta musi projit seznam vzdycky cely.

-- Ukazme si mensi trik pro definovani Fibonacciho cisel. To, co se v nem deje
-- je, ze se scita seznam Fibonacciho cisel se seznamem o jedna posunutym, tim
-- dostaneme vysledny seznam. Cele to opet funguje jen diky linemu vyhodnoceni.

fibs :: [Integer]
fibs = 0:1:(zipWith (+) fibs (tail fibs))

-- Obcas se nam muze hodit skladat vice funkci dohromady. Mame nekolik moznosti,
-- jak to udelat. Minule jsme pouzivali tu moznost, ze slozeni funkci `f` a
--  `g` jsme psali jako `f (g x)`. To same se da zapsat pomoci `f $ g x` 
-- (`$` se v zasade chova jako zavorka od mista, kde je, az do konce vyrazu), 
-- pripadne `(f . g) x`.

prictiJedna x = x+1
vynasobDvema x = 2*x

-- `prictiJedna (vynasobDvema 3)`, je tedy to same, jako 
-- `prictiJedna $ vynasobDvema 3` a take jako `(prictiJedna . vynasobDvema) 3`. 
-- Hlavni rozdil mezi `$` a `(.)` je, ze `(.)` se aplikuje primo na dve funkce 
-- a vytvari novou funkci. Na druhou stranu jeho nevyhoda je, ze funguje jen na
-- funkce s jednim parametrem vpravo.

-- Diky skladani funkci si muzeme usetrit trochu psani a napsat nektere veci 
-- strucneji. Napriklad nase Hornerovo schema jde napsat take jako

horner2 :: Num a => [a] -> a
horner2 = foldl ((+).(*10)) 0

-- Krome kratsiho kodu to muze ucinit kod o neco citelnejsim, a obcas to 
-- dokonce pomuze kompilatoru v optimalizaci. Styl programovani, kde v definici
-- funkce nejsou promenne se nazyva point-free programming. Pokud se to prehani,
-- vznikly kod nemusi byt uplne dobre citelny, proto si tento styl vyslouzil
-- take prezdivku pointless programming. Neprehanejte to s point-free, ale
-- nebojte se ho pouzit, a hlavne ho umejte cist. Posledni parametry, pokud to
-- jde, se vynechavaji skoro vzdy.

-- Minule jsme psali skalarni soucin dvou vektoru pomoci zipWith a sum.

skalarniSoucin1 x y = sum (zipWith (*) x y)

-- Za pomoci operatoru pro aplikaci funkce si muzeme usetrit par zavorek a to
-- same napsat take jako 

skalarniSoucin2 x y = sum $ zipWith (*) x y

-- Relativne snadno se take muzeme zbavit posledniho `y`.

skalarniSoucin3 x = (sum . zipWith (*) x)

-- Udelat ze skalarniho soucinu kompletne point-free funkci uz tak snadne neni.
-- Prijdete na to, jak ji vytvorit? (Opet je to spis tezka otazka, pomoci by
-- vam mohl nasledujici operator, co dela?)

(.:) = (.).(.)

-- (Kdo by chtel googlit, tak na forech se tomuto operatoru neformalne rika
-- boobs operator, nebo owl operator.)