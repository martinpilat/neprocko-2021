-- Author: Martin Pilat
-- Date: 16. 5. 2017
-- Updated: 18. 5. 2020

------------------------------------------------------------------------------
--          6. cviceni -- Haskell - State, IO, record syntaxe, zipper
------------------------------------------------------------------------------

-- V dnesnich cvicenich se nam hodi nekolik importu:

import Data.Char
import Control.Exception
import Network.HTTP -- pokud modul nemate, muzeho ho nainstalovat pomoci `cabal update; cabal install HTTP`

-- Rikali jsme si, ze funkce v Haskellu jsou ciste, tj. jejich hodnoty zavisi 
-- jen na jejich vstupnich parametrech. To je sice pekne a dobre se s tim 
-- pracuje, ale obcas to je neprakticke. Predstavme si treba, ze chceme v 
-- pracovat se zasobnikem cisel. Muzeme si ho nadefinovat jako

type Zasobnik = [Int]

-- Pro praci se zasobnikem potom pouzivame typicky funkce jako `push` a `pop`. 
-- `push` pridava hodnotu na zasobnik a `pop` vraci hodnotu z vrcholu zasobniku 
-- (a odebere ji). Pro nas zasobnik je jednoduche je napsat.

push::Int -> Zasobnik -> Zasobnik
push h zas  = h:zas

pop::Zasobnik -> (Int, Zasobnik)
pop (z:zas) = (z, zas) 

-- Jak bychom ted napsali funkci, ktera odebere dve hodnoty z vrcholu zasobniku,
-- secte je, a vrati novy zasobnik?

zasobnikTest::Zasobnik->(Int, Zasobnik)
zasobnikTest z = (a+b, nz)
        where
            (a, z1)   = pop z
            (b, z2)   = pop z1
            nz        = push (a+b) z2

-- Neni to tezke, ale je to celkem ukecane a neprehledne, musime se sami starat 
-- o predavani zasobniku mezi funkcemi. Pritom by bylo tak pekne, kdybychom 
-- mohli pouzivat treba `do` notaci a zasobniku si vubec nevsimat. Takova vec 
-- je v Haskellu mozna a implementuje ji `State` monada. Ta ma dva typove 
-- parametry, typ stavu (`s`) a typ navratove hodnoty funkce (`a`). Zajimave 
-- je, ze uvnitr monady tentokrat nemame samotne hodnoty techto dvou typu, ale
-- funkci, ktera vezme stav a spocita navratovou hodnotu a novy stav. Musi to
-- tak byt, jinak bychom se museli o stav starat porad sami a predavat si ho.
-- Cela myslenka teto monady je v tom, ze budeme postupne skladat funkce. Na
-- zacatku zadame jako parametr pocatecni stav a dal se o nej funkce budou
-- starat samy a budou si ho i samy predavat (pomoci `>>=`).

data Stav s a = Stav {runState :: s -> (a, s)}

-- V definici `Stav` si muzete vsimnout pouziti tzv. record syntaxe, ta vlastne
-- pojmenovava funkci ulozenou uvnitr `Stav` a zaroven vytvari funkci
-- `runState::Stav s a -> s -> (a, s)`, ktera nam vrati funkci obsazenou ve
-- `Stav`. Funkce `pop` a `push` potom muzeme prepsat tak, aby pracovaly se
-- stavem, ktery obsahuje nas zasobnik. Potrebujeme tedy, aby vracely neco typu
-- `Stav Zasobnik b`, kde `b` je libovolny navratovy typ funkce. U `popS` tento
-- typ je `Int` (prvni hodnota ze zasobniku), u `pushS` pouzijeme jako typ `()`,
-- kteremu se take rika unit. Je to typ, ktery muze obsahovat jen jednu hodnotu
-- a to `()`. Odpovida trochu typu void v jinych programovacich jazycich
-- (`NoneType` v Pythonu). (Pozor v Haskellu je i typ `Void`, ktery dela neco
-- uplne jineho -- je pro funkce, ktere nikdy nic nevrati.)

popS::Stav Zasobnik Int
popS = Stav $ \(s:ss) -> (s,ss)

-- `popS` tedy zabali do `Stav` funkci, ktera vezme stav, z nej odebere prvni
-- prvek ten nastavi jako vyslednou hodnotu a vrati zasobnik bez prvniho prvku.

pushS::Int -> Stav Zasobnik ()
pushS x = Stav $ \ss -> ((),x:ss)

-- Na `pushS` vidime, ze funkce pro praci se stavem mohou mit i parametry. V
--  tomto pripade `pushS` dostane `Int`, ten prida na zacatek zasobniku a vrati
-- `()`. Neni totiz asi zadna rozumna hodnota, kterou by `pushS` jinak mohla
-- vratit, tato navratova hodnota nas navic nebude zajimat. Pojdme ze `Stav`
-- udelat monadu: samotna monada musi mit jen jeden typovy parametr,
-- implementovat jako monadu tedy budeme `(Stav s)` a ne jen samotny `Stav`.

-- Prvni vec, kterou potrebujeme je napsat instanci pro `Functor (Stav s)`,
-- tzn. potrebujeme napsat funkci `fmap::(a -> b) -> Stav s a -> Stav s b`,
-- mela by to tedy byt funkce, ktera vezme funkci `a->b`, ktera stav uplne
-- ignoruje, a aplikuje ji ve `Stav s`. Jedina rozumna implementace takove
-- funkce bude vzit navratovou hodnotu ze `Stav s a`, na ni aplikovat funkci a
-- vytvorit tak `Stav s b`. Samotny vnitrni stav by se nemel zmenit.

instance Functor (Stav s) where
    -- fmap::(a -> b) -> Stav s a -> Stav s b
    fmap f x = Stav $ \s -> let (v, ns) = runState x s 
                            in  (f v, ns)

-- Na implementaci je mozna vhodne podivat se podrobneji. `runState x` vytahne
-- funkci ze stavu `x` (druheho parametru `fmap`) a aplikuje ji na stav
-- `s` (parametr nove funkce, kterou definujeme), tim ziska vysledek a stav, na
-- vysledek potom aplikuje `f` (prvni parametr `fmap`) a stav jen zkopiruje.

-- Jako dalsi krok potrebujeme napsat instanci `Applicative` pro `Stav s`, tedy
-- funkci `pure::a -> Stav s a`, ktera co nejjednoduseji zabali hodnotu do
-- `Stav s`, a operator `(<*>)::Stav s (a->b) -> Stav s a -> Stav s b`.

-- Funkce `pure` je jednoducha, kdyz dostane hodnotu, tak vytvori funkci, ktera
-- jen zkopiruje stav a jako navratovou hodnotu nastavi hodnotu ze vstupu.

-- Operator `(<*>)` je o neco komplikovanejsi. Napred spusti `runState` na stav,
-- ktery obsahuje funkci `(a->b)`, potom spusti `runState` i na druhy parametr,
-- tim dostane vysledek typu `a` a dalsi novy stav. Na vysledek aplikuje funkci,
-- ktera byla vysledkem prvniho stavu a vrati nejnovejsi stav.

instance Applicative (Stav s) where
    -- pure:: a -> Stav s a
    pure x     = Stav $ \s -> (x, s)
    -- (<*>)::Stav s (a->b) -> Stav s a -> Stav s b    
    sab <*> sa = Stav $ \s -> let (ab, ns) = runState sab s
                                  (a, ns2) = runState sa ns
                              in  (ab a, ns2)

-- Konecne muzeme napsat instanci `Monad (Stav s)`, staci uz nam jen dodefinovat
-- operator `(>>=)::Stav s a -> (a -> Stav s b) -> Stav s b`, ktery vlastne
-- retezi dve funkce se stavem za sebou. Napred tedy spusti `runState` na svou
-- levou stranu, a na vysledny stav aplikuje funkci, kterou ma na prave strane.
-- Tim dostane novy stav a zavola na nej `runState`.

instance Monad (Stav s) where
    --(>>=)::Stav s a -> (a -> Stav s b) -> Stav s b
    h >>= f = Stav $ \s -> let (a, newState) = runState h s
                               g             = f a 
                           in  runState g newState

-- Nyni muzeme nasi testovaci funkci prepsat jednoduse pomoci `do` notace, je
-- videt, ze kod je mnohem prehlednejsi a snadnejsi na pochopeni, navic se
-- nemusime o stav starat sami.

zasobnikTestS::Stav Zasobnik Int
zasobnikTestS = do
    a <- popS
    b <- popS
    pushS (a+b)
    return (a+b)

-- Pomoci `State` monady muzeme treba implementovat i praci s nahodnymi cisly.
-- Napiseme si jednoduchy linearni kongruencni generator nahodnych cisel.

type Rand t = Stav Int t

rand::Int -> Rand Int
rand max = Stav $ \s -> let x = (s*1664525+1013904223) `mod` 2^32 
                        in      (x `mod` max, x)

nahoda::Int -> Int -> Rand [Int]
nahoda 1 max = fmap pure $ rand max
nahoda n max = (:) <$> rand max <*> nahoda (n-1) max

seed::Int -> Rand ()
seed n = Stav $ \_ -> ((), n)

-- Jedna z nejdulezitejsich monad v Haskellu je `IO`. V teto monade zije
-- vsechno, co chce nejakym zpusobem komunikovat s vnejsim svetem. Muzete si to
-- predstavovat treba tak, ze ta monada se stara o to, aby si jednotlive funkce
-- mezi sebou predavaly nejaky stav. A ten stav je v tomto pripade cely vnejsi
-- svet (vnejsi z pohledu programu). Diky tomu potom funkce mohou menit tento
-- stav (vystup), pripadne z toho stavu neco zjistovat (vstup). Tim jsme dosahli
-- toho, ze se nam podarilo propojit svet funkci bez vedlejsich efektu s
-- pozadavkem na to, mit vstupy a vystupy.

-- V Haskellu je bezne, ze se vstupy a vystupy oddeluji od vlastniho provadeni
-- programu. Monada `IO` by vam tedy nemela nikdy prolezt celym programem az k
-- typum pomocnych funkci nekde hluboko v kodu. Naopak, `IO` by ve svem typu
-- mely mit jen funkce, ktere vstup a vystup nutne potrebuji delat. Podle toho,
-- ze nejaka funkce ma ve svem typu IO poznate, ze muze mit nejake vedlejsi
-- efekty.

-- Podivejte se treba na funkci `getLine`, jeji typ je `IO String`. To znamena,
-- ze tahle funkce udela nejake vstupy a vystupy a vrati nakonec `String`. Diky
-- tomu, ze tento `String` ma pred sebou jeste IO vidime, ze muze byt jiny pri
-- kazdem volani teto funkce - funkce tedy ma vedlejsi efekty.

-- Uz jsme videli i funkci `putStrLn`, ta ma typ `String -> IO ()`, to znamena,
-- ze vezme `String`, udela nejaky vstup a vystup a nic nevraci (tj. jen meni
-- okolni svet).

-- My samozrejme vime presne, co tyhle funkce delaji - `getLine` nacte jednu
-- radku ze vstupu, `putStrLn` vypise `String` na standardni vystup.

-- Napiste ted program, ktery se zepta uzivatele na nejaky text a potom ho
-- postupne po radkach nacita a vypisuje na konzoli velkymi pismeny. (Ve
-- skutecnosti ho nacte cely najednou, ale buffer je nastaveny defaultne po
-- radkach, takze to vzdy po konci radky zpracuje.) Pokud byste chteli nacist 
-- jen jednu radku, muzete pouzit `getLine`. Jeden znak se nacte pomoci
-- `getChar`. Program ukoncete pomoci Ctrl+C.

zakric :: IO ()
zakric = do 
        putStrLn "Zadej text: "
        text <- getContents
        let output = naVelka text
        putStrLn output

naVelka :: [Char] -> [Char]        
naVelka = map (toUpper)

-- Vsimnete si nekolika veci: `zakric` ma typ `IO ()`, tj. je to funkce, ktera
-- ma nejake vedlejsi efekty a nakonec nic nevraci; `naVelka` je cista funkce,
-- ktera ve svem typu zadne `IO` nema, taky by mit nemela, nedela zadne vstupy
-- a vystupy a nakonec je ji uplne jedno, kde ten `String`, se kterym pracuje,
-- vezme; v `do` blocich se za `let` nepise `in`. Pokud chceme v `do` bloku
-- pojmenovat vystup z ciste funkce pouzijeme `let`, pokud z nejake IO akce
-- pouzijeme `<-`.

-- Nacteni vstupu, jeho zpracovani a vystup jsou tak bezne cinnosti, ze pro ne
-- dokonce existuje hotova funkce, jmenuje se `interact` a pomoci ni muzeme
-- prepsat predchozi priklad takhle:

iZakric :: IO ()
iZakric = do
    putStrLn "Zadej text: "
    interact naVelka

-- Tim, ze se nam podarilo oddelit vstup a vystup od jejich zpracovani se nam
-- zaroven podarilo to, ze samotne funkce, ktere vstupy a vystupy zpracovavaji
-- vubec nemusi vedet (ani nevi), odkud se data vzala. Mohla se klidne vzit i
-- z nejakeho souboru.

-- Muzeme si treba podobnym zpusobem napsat program, ktery nacte ze souboru 
-- cisla a na vystup vypise jejich soucet.

secti :: String -> IO ()
secti vstup = do
    vstup <- readFile vstup
    let vystup = zpracujSoucet vstup    
    putStrLn vystup

zpracujSoucet :: String -> String    
zpracujSoucet v = show $ sum $ map (\x -> (read x)::Integer) $ lines v

-- Nacteni celeho souboru najednou, jak je udelano v prikladu vyse, se bat
-- nemusite. Pokud ho zpracovavate rozumne, Haskell ho cely najednou v pameti
-- nikdy mit nebude. Jakmile zjisti, ze nektera data uz nepotrebuje, zahodi je.
-- Zpracovani v prikladu uplne rozumne neni, `sum` totiz pouziva `foldl` a ten
-- potrebuje mit napred cely seznam cisel. Pokud bychom misto `foldl` v `sum`
-- pouzili `foldl'` potom by Haskell cely soubor zpracoval v konstantni pameti.

-- Pokud soubor neexistuje, Haskell vyhodi vyjimku a pokud ji nikdo neodchyti,
-- tak spadne. Haskell na vyjimky reagovat umi, neni s tim problem. My se tim
-- moc zabyvat nebudeme, jen si povime o jedne funkci: `handle`

sectiExceptions vstup = handle ((\_ -> putStrLn "Soubor se nepodarilo otevrit")::IOException -> IO ()) $ do
        vstup <- readFile vstup
        let vystup = zpracujSoucet vstup
        putStrLn vystup

-- Podobnym zpusobem muzeme cist i data ze site (potrebujeme modul 
-- `Network.HTTP`). Priklad funguje jen pro ASCII soubory, zkousejte treba s
-- `http://www.google.com/robots.txt`.

stahni url fileName = do
            response <- simpleHTTP $ getRequest url
            print response
            let body = fmap rspBody response
            case body of 
                Left _          -> putStrLn "error"
                Right content   -> writeFile fileName content

-- V predchozim prikladu jste zaroven videli typicke pouziti typu
-- `Either a b = Left a | Right b`. V pripade chyby se jeji popis ulozi do
-- `Left`, pokud se chyba nestane, je vysledek v `Right`. Jedna se vlastne o
-- rozsireni typu `Maybe` (a funguje podobne, navic je to taky monada).

-- `ghc` umi Haskell i kompilovat, k tomu je dulezite, abychom meli metodu 
-- `main::IO ()`. Typ `IO ()` rika, ze ta metoda pracuje se vstupy a vystupy
-- (obecne s IO monadou) a nic nevraci. V modulu `System.Environment` je metoda
-- `getArgs :: IO [String]`, ktera vraci seznam parametru z prikazove radky
-- (bez nazvu programu, ten muzete dostat pomoci `getProgName :: IO String`).

-- Typicky `main` obsahuje nacteni vstupu (pripadne i parametru) a jejich
-- predani cistym funkcim, ktere uz `IO` nepouzivaji. Nakonec se zase vysledky
-- techto cistych funkci vypisou (typicky zase primo v `main` je volani
-- vypisovacich funkci).

-- V Haskellu vstup a vystup funguje striktne (tj. ne line), ale streamovane.
-- To znamena, ze data programem v Haskellu protecou, a pokud nejsou vsechna
-- najednou treba, v pameti se neulozi. Striktnost vstupu a vystupu je dulezita
-- jinak by vysledek programu mohl zalezet na poradi vyhodnoceni (napr. dvakrat
-- `getLine` za sebou by vracel jine vysledky, kdyby se druhy vyhodnotil drive
-- nez prvni).

-- Defaultni chovani `ghc` pri kompilovani je nepouzivani optimalizaci. Pokud
-- chcete optimalizace zapnout, staci mu dat parametr `-O`, pripadne `-O1` 
-- (znamena to same). `-O2` muze pouzit i "nebezpecne" optimalizace, ktere
-- mohou pri trose smuly vest naopak ke zpomaleni kodu. Dokumentace ke `ghc`
-- tvrdi, ze `-O2` malokdy vytvori rychlejsi kod nez `-O`.

-- Jak velke je zrychleni? Pokud napr. pouzijeme kod, ktery jsme ukazovali
-- vyse (soucet hodnot ze souboru) na secteni milionu cisel, zjistime, ze
-- bez optimalizaci to trva cca 5.5 sekundy, s nimi jen 4.5 sekundy. Navic s
-- optimalizacemi vystacime s mensim zasobnikem (default je 8MB), bez nich ho
-- potrebujeme zvysit.

-- Jeste vetsi rozdil uvidime, kdyz ho spustime na prvnich 100M cisel. Verze,
-- ktera pouziva `-O` dobehne za 433 sekund a po celou dobu zabira cca 3MB 
-- pameti. Oproti tomu verze bez optimalizaci sezere 1GB pameti a pak spadne
-- (muzeme ji dat pameti vice, ale stejne to nepomuze). Rozdil je v tom, ze GHC
-- optimalizuje striktnost vypoctu a je schopne detekovat, ze `sum` (coz je
-- vlastne `foldl (+) 0`) se da vyhodnotit v konstantnim prostoru (v zasade z
-- `foldl` udela `foldl'` a navic se postara o to, aby se soubor nenacital cely,
-- ale jen kousky, ktere jsou zrovna potreba).

-- Predesla diskuze neni rozhodne sofistikovany benchmark, ma spis ukazat, ze
-- se vyplati Haskellovske programy kompilovat a pokud mate nejaky problem, je
-- vhodne zapnout optimalizace.

-- Zkusme si jeste nakonec naimplementovat simulator nedeterministickeho
-- Turingova stroje. Ukazeme si na tom nekolik malo technik, ktere jsme jeste
-- nevideli a procvicime si praci s nedeterminismem.

-- Prvni co budeme potrebovat je nejaky zpusob, jak reprezentovat pohyb hlavy
-- na paskach.

data Smer = L | R | N

-- Dale budeme potrebovat nejakou prechodovou funkci.

data Fce a b = Fce [((a,b),(a,b,Smer))]

-- A potom samotny Turinguv stroj. Tady si vsimnete definice pasky: jsou tam
-- dva seznamy a jedna hodnota. Hodnota urcuje symbol pod hlavou TS. Levy
-- seznam je paska vlevo od hlavy (ulozena pozpatku, takze na zacatku seznamu
-- je symbol, ktery je hned vlevo od hlavy) a v pravem seznamu je paska napravo
-- od hlavy.

--data TS a b = TS ([a],a,[a]) b (Fce a b) [b] Int Int Int        

data TS a b = TS {
          paska :: ([a],a,[a])  -- obousmerne nekonecna paska TS
        , stav :: b             -- aktualni stav TS
        , fce :: Fce a b        -- prechodova fce
        , koncoveStavy :: [b]   -- seznam koncovych stavu
        , pos :: Int            -- aktualni pozice od zacatku (pro vypis)
        , minPos :: Int         -- nejlevejsi pozice (pro vypis)
        , maxPos :: Int         -- nejpravejsi pozice  (pro vypis)
        }

-- Pouzili jsme tzv. record syntaxi, ktera nam krome definice dat zaroven
-- definuje i funkce, ktere jednotlive polozky vraci. Takze napr. zavolani
-- `paska ts`, kde `ts` je nejaky TS nam vrati jeho pasku.

-- Jeste se nam hodi funkce, ktera vrati symbol pod hlavou TS. Vsimnete si
-- triku s `where`, ktery nam umoznuje udelat pattern matching v tele funkce.

hlava :: TS a b -> a
hlava ts = a where (_,a,_) = paska ts

-- Funkce, ktera "pekne" vypise TS. Tentokrat jsme misto triku s `where`
-- pouzili podobny trik s `let`.

showTS:: (Show a, Show b) => TS a b -> String
showTS ts = let 
                (xs,h,ys)   = paska ts
                b           = stav ts
                right       = (maxPos ts) - (pos ts)
                left        = (pos ts) - (minPos ts)
            in
                show (reverse $ take left xs) ++ show h ++ show (take right ys) ++ "-" ++ show b

-- Funkce, ktera vraci vsechny moznosti prechodove funkce aplikovatelne v
-- danem stavu s danym symbolem pod hlavou.

findAll :: (Eq a, Eq b) => (a,b) -> Fce a b -> [(a,b,Smer)]
findAll (a,b) (Fce f) = [(x,y,s) | ((a1,b1),(x,y,s)) <- f, a==a1, b==b1]

-- Ted je potreba naimplementovat jeden krok TS. Funkce je sice dlouha, ale
-- neni moc tezka. Vetsina prace a triku se deje az v case na konci.

step :: (Eq a,Eq b) => TS a b -> [TS a b]
step ts = do                                          -- vyuzijeme nedeterminismu list monady
            let f = fce ts                            -- prechodova fce
            let ((x:xs), p, (y:ys)) = paska ts        -- paska a symbol pod hlavou
            let s = stav ts                           -- stav
            let kroky = findAll (hlava ts, stav ts) f -- mozne prechody
            let ps = pos ts                           -- aktualni pozice na pasce
            let minP = minPos ts                      -- nejlevejsi navstsivena pozice
            let maxP = maxPos ts                      -- nejpravejsi navstivena pozice
            (np, ns, d) <- kroky                      -- jeden mozny krok
            case d of                                 -- posun a zmena stavu podle kroku
                L -> return ts {paska = (xs,x,np:y:ys), stav = ns, pos = ps - 1, minPos = min minP (ps - 1)}
                R -> return ts {paska = (np:x:xs,y,ys), stav = ns, pos = ps + 1, maxPos = max maxP (ps + 1)}
                N -> return ts {paska = (x:xs,np,y:ys), stav = ns} 

-- Vsimnete si poslednich trech radek. Record syntaxe nam umoznuje vytvorit
-- upravenou kopii TS, aniz bychom museli opisovat zbytek informaci, ktere se
-- nemeni. To se hodi, nemusime totiz prepisovat vsechny funkce, kdyz se
-- rozhodneme pridat do TS nejakou dalsi informaci.

-- Nakonec napiseme samotnou simulaci. Ta uz je snadna. Stroje, ktere jsou v
-- nejakem koncovem stavu uz se dal nesimuluji. Na ostatni se aplikuje funkce
-- `step`. Funkce opet vyuziva nedeterminismu `list` monady.

sim::(Eq a, Eq b) => TS a b -> Int -> [TS a b]
sim ts 0    = return ts
sim ts maxS = do
                stroj <- step ts
                if (stav stroj) `elem` (koncoveStavy stroj) then
                    return stroj
                else
                    sim stroj (maxS - 1)

bb2 = TS (repeat 0, 0, repeat 0) 'a' (Fce [((0, 'a'),(1, 'b', R)), ((0, 'b'), (1, 'a', L)), ((1, 'a'), (1,'b',L)), ((1,'b'),(1,'h',R))]) ['h'] 0 0 0 :: TS Int Char

instance (Show a, Show b) => Show (TS a b) where
    show = showTS

-- Zpusob, jakym jsme reprezentovali pasku TS se nazyva zipper. Umoznuje nam
-- pristup v konstantnim case na jedno konkretni misto v datove strukture (v
-- tomhle pripade v seznamu). Krome seznamu se da pouzit i na stromu (tam je
-- treba si pamatovat, na jake strane byl dany kousek prilepen). Muzete si ho
-- predstavit tak, ze strom (seznam) vezmete za to misto, kam chcete mit rychly
-- pristup a zvednete ho do vzduchu, co spadne dolu na jednu stranu je jedna
-- polovina zipperu, co spadne na druhou je jeho druha polovina (u TS jsme si
-- navic pamatovali prvek, za ktery jsme ho drzeli).