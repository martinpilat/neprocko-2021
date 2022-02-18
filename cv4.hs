-- Author: Martin Pilat
-- Date: 2. 5. 2017
-- Updated: 11. 5. 2020

------------------------------------------------------------------------------
--          4. cviceni -- Stromy, obecny fold
------------------------------------------------------------------------------

-- Kdo to jeste neuhodl, tak cele nase povidani o typech vede k tomu, abychom si
-- nadefinovali vlastni binarni strom.

data Strom a = Nil | Uzel (Strom a) a (Strom a) deriving (Show)

-- Strom tedy je bud `Nil`, nebo `Uzel`, ktery obsahuje dva podstromy a jednu 
-- hodnotu typu `a`.

-- Ted by se nam moc hodilo napsat si binarni vyhledavaci strom. Zacneme napred
-- pridanim do stromu.

pridej :: Ord a => Strom a -> a -> Strom a
pridej Nil y                        = Uzel Nil y Nil
pridej (Uzel x b y) z   | z < b     = Uzel (pridej x z) b y
                        | otherwise = Uzel x b (pridej y z)
                                        
-- Ted uz je jednoduche prevest seznam na strom. 

seznamNaBST :: Ord a => [a] -> Strom a
seznamNaBST = foldl pridej Nil
                        
-- Videli jsme, ze pro praci se seznamy je velmi vhodne mit funkci `map`. 
-- Napisme si podobnou funkci i pro stromy.

treeMap :: (a -> b) -> Strom a -> Strom b
treeMap _ Nil           = Nil
treeMap f (Uzel l u p)  = Uzel (treeMap f l) (f u) (treeMap f p)

-- Vratme se na chvili k seznamum, a zamysleme se nad tim, co vlastne jsou. 
-- Muzeme si napsat vlastni seznam.

data Seznam a = SNil | Cons a (Seznam a) deriving Show

-- `SNil` nam oznacuje prazdny seznam, `Cons a (Seznam a)` je konstruktor, 
-- ktery spojuje jeden prvek se seznamem (v Haskellu `(:)`).

-- Aby se nam lepe s nasim seznamem pracovalo, napisme si funkci, ktera prevadi 
-- Haskellovsky seznam na ten nas. 

listNaSeznam :: [a] -> Seznam a
listNaSeznam = foldr Cons SNil

-- Definice je tak snadna, protoze jen potrebujeme nahradit `(:)` za `Cons` a 
-- `[]` za nas `SNil`.

-- Duvod, proc tuhle odbocku delame, je, abychom si ukazali, jak vypada `foldr`
-- na tomhle seznamu.

mujFoldr :: (a -> b -> b) -> b -> Seznam a -> b
mujFoldr _ fNil SNil            = fNil
mujFoldr fCons fNil (Cons x xs) = fCons x (mujFoldr fCons fNil xs)

-- Pomoci maleho triku muzeme funkci zjednodusit. Funkce `g` tady zastupuje
-- `mujFoldr2 fCons fNil` a tim si usetrime opisovani parametru.

mujFoldr2 :: (a -> b -> b) -> b -> Seznam a -> b
mujFoldr2 fCons fNil = g where
                            g SNil          = fNil
                            g (Cons x xs)   = fCons x (g xs)

-- Tady je dulezite si uvedomit, co ta funkce presne dela. Podiva se na oba 
-- datove konstruktory naseho seznamu (`SNil` a `Cons`) a nahradi je volanim 
-- nejake funkce, ktera ma stejny typ, jako dany konstruktor. 

-- Zkusme tedy napsat fold pro stromy. V tomto pripade mame opet dva 
-- konstruktory - `Nil` a `Uzel a b c`. Budeme tedy take potrebovat dve funkce.
-- Jedna z nich bude nularni a bude rikat, cim se ma nahradit `Nil`. Druha bude
-- mit tri parametry, a bude rikat,  co se ma stat s kazdym uzlem. Vysledny typ
-- funkce `treeFold` tedy bude:

treeFold :: (a -> b -> a -> a) -> a -> Strom b -> a
treeFold _      fNil Nil            = fNil
treeFold fUzel  fNil (Uzel l u p)   = fUzel (treeFold fUzel fNil l) u (treeFold fUzel fNil p)

-- Tahle definice foldu je trochu moc ukecana, da se zase zkratit stejnym
-- trikem jako u seznamu.

treeFold2 :: (a -> b -> a -> a) -> a -> Strom b -> a
treeFold2 fUzel fNil =  g where
                            g Nil           = fNil
                            g (Uzel l u p)  = fUzel (g l) u (g p)

-- Porovnejte tyto dva foldy. Co maji spolecneho? Oba nahrazuji vsechny datove
-- konstruktory sveho typu volanim nejake funkce. Pokud budete tedy chtit napsat
-- fold pro nejaky vlastni typ, staci udelat to same.
                            
-- Se stromovym foldem se zase da delat spousta zabavnych veci. Napriklad je
-- uplne jednoduche vypsat strom v prefixovem zapisu.

prefixBSTnaSeznam :: Strom a -> [a]
prefixBSTnaSeznam = treeFold (\la u pa -> la ++ (u:pa)) []

-- Co ta funkce dela? Nahradi `Nil` prazdnym seznamem, a potom ma vlastne dva
-- akumulatory, jeden obsahuje levou cast, druhy pravou. Funkce je oba vezme a
-- spoji je s prvkem v aktualnim uzlu.

-- Zkuste sami napsat infixovou a postfixovou variantu tohoto prevodu.

-- Jak byste nadefinovali strom, ktery muze reprezentovat aritmeticky vyraz s
-- binarnimi operacemi?

-- Muzeme si napsat vlastni `show` funkci, ktera bude prevadet `Strom` na
-- `String`. Typova trida `Show` nam takovou funkci nadefinuje sama (kdyz
-- pouzijeme `deriving Show`), nebo si ji muzeme napsat sami, jako nize. Neni
-- mozne oba zpusoby kombinovat, proto definujeme novy typ stromu `StromS`.

-- Funkce neni nijak extra, kresli strom zleva doprava, cim dale vpravo cislo
-- je, tim je hloubeji ve strome.

data StromS a = NilS | UzelS (StromS a) a (StromS a)

showTree :: Show a => StromS a -> [String]
showTree (NilS) = []
showTree (UzelS l u p) = map (\x -> "   " ++ x) ((showTree p) ++ [show u] ++ (showTree l))

instance Show a => Show (StromS a) where
    show t = unlines (showTree t)