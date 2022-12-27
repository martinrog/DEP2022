{-|
    Module      : Lib
    Description : Checkpoint voor V2DEP: recursie en lijsten
    Copyright   : (c) Brian van de Bijl, 2020
    License     : BSD3
    Maintainer  : nick.roumimper@hu.nl

    In dit practicum oefenen we met het schrijven van simpele functies in Haskell.
    Specifiek leren we hoe je recursie en pattern matching kunt gebruiken om een functie op te bouwen.
    LET OP: Hoewel al deze functies makkelijker kunnen worden geschreven met hogere-orde functies,
    is het hier nog niet de bedoeling om die te gebruiken.
    Hogere-orde functies behandelen we verderop in het vak; voor alle volgende practica mag je deze
    wel gebruiken.
    In het onderstaande commentaar betekent het symbool ~> "geeft resultaat terug";
    bijvoorbeeld, 3 + 2 ~> 5 betekent "het uitvoeren van 3 + 2 geeft het resultaat 5 terug".
-}

module Lib
    ( ex1, ex2, ex3, ex4, ex5, ex6, ex7
    ) where

-- TODO: Schrijf en documenteer de functie ex1, die de som van een lijst getallen berekent.
-- Voorbeeld: ex1 [3,1,4,1,5] ~> 14

{-| Deze functie berekent de som van een lijst met getallen. Dit gebeurd met behulp van recursie. Je 
pakt het eerste getal van een lijst, die tel je op bij de rest van de lijst (wat aan wordt geroepen door
dezelfde functie)
-}

ex1 :: [Int] -> Int
ex1 [] = 0
ex1 (x:xs) = x + ex1 xs 

-- TODO: Schrijf en documenteer de functie ex2, die alle elementen van een lijst met 1 ophoogt.
-- Voorbeeld: ex2 [3,1,4,1,5] ~> [4,2,5,2,6]

{-| Deze functie verhoogt elk item van de lijst met 1. Dit gebeurd met behulp van recursie. Je 
pakt het eerste getal van een lijst en hier tel je 1 bij op, vervolgens plak je de rest van de lijst
hieraan vast (wat aan wordt geroepen door dezelfde functie)
-}

ex2 :: [Int] -> [Int]
ex2 [] = []
ex2 (x:xs) = x + 1 : ex2 xs


-- TODO: Schrijf en documenteer de functie ex3, die alle elementen van een lijst met -1 vermenigvuldigt.
-- Voorbeeld: ex3 [3,1,4,1,5] ~> [-3,-1,-4,-1,-5]

{-| Deze functie vermenigvuldigt elk item van de lijst met -1. Dit gebeurd met behulp van recursie. Je 
pakt het eerste getal van een lijst en vermenigvuldig je met -1, vervolgens plak je de rest van de lijst
hieraan vast (wat aan wordt geroepen door dezelfde functie)
-}

ex3 :: [Int] -> [Int]
ex3 [] = []
ex3 (x:xs) = x * (-1):ex3 xs

-- TODO: Schrijf en documenteer de functie ex4, die twee lijsten aan elkaar plakt.
-- Voorbeeld: ex4 [3,1,4] [1,5] ~> [3,1,4,1,5]
-- Maak hierbij geen gebruik van de standaard-functies, maar los het probleem zelf met (expliciete) recursie op. 
-- Hint: je hoeft maar door een van beide lijsten heen te lopen met recursie.

{-| Deze functie plakt 2 lijsten aan elkaar vast. Dit gebeurd met behulp van recursie. Je pakt het eerste
 getal van  de eerste lijst, dit plak je vast aan het volgende: de functie wordt aangeroepen met het restant
 van de eerste lijst en de gehele tweede lijst. Zo krijg je uiteindelijk één lijst, waar twee lijsten achter
 elkaar zijn geplakt.
-}

ex4 :: [Int] -> [Int] -> [Int]
ex4 [] [] = []
ex4 (lst) [] = lst
ex4 [] (lst) = lst
ex4 (x:xs) (lst) = x : ex4 xs lst

-- TODO: Schrijf en documenteer een functie, ex5, die twee lijsten van gelijke lengte paarsgewijs bij elkaar optelt.
-- Voorbeeld: ex5 [3,1,4] [1,5,9] ~> [4,6,13]

{-| Deze functie telt uit twee aparte lijsten elk getal, paarsgewijs, bij elkaar op. Dit doet hij door de 
het eerste item van lijst 1 (x) + het eerste item uit lijst 2 (y) te doen. Vervolgens word dit proces herhaalt
met de rest van de lijsten (m.b.v. recursie).
 -}

ex5 :: [Int] -> [Int] -> [Int]
ex5 [] [] = []
ex5 (lst) [] = lst
ex5 [] (lst) = lst
ex5 (x:xs) (y:ys) = x + y : ex5 xs ys

-- TODO: Schrijf en documenteer een functie, ex6, die twee lijsten van gelijke lengte paarsgewijs met elkaar vermenigvuldigt.
-- Voorbeeld: ex6 [3,1,4] [1,5,9] ~> [3,5,36] 

{-| Deze functie vermenigvuldigt uit twee aparte lijsten elk getal met elkaar, ook dit gebeurd paarsgewijs.
Ook deze functie gebeurd op dezelfde manier als ex5, alleen worden de x en y met elkaar vermenigvuldigt ipv
opgeteld. 
-}

ex6 :: [Int] -> [Int] -> [Int]
ex6 [] [] = []
ex6 (lst) [] = lst
ex6 [] (lst) = lst
ex6 (x:xs) (y:ys) = x * y : ex6 xs ys

-- TODO: Schrijf en documenteer een functie, ex7, die de functies ex1 en ex6 combineert tot een functie die het inwendig product uitrekent.
-- Voorbeeld: ex7 [3,1,4] [1,5,9] geeft 3*1 + 1*5 + 4*9 = 44 terug als resultaat.

{-| Deze functie berekent het inwendig product van twee lijsten. Dit gebeurd door eerst de functie ex6 op de
lijsten toe te passen. Dus alles paarsgewijs vermenigvuldigen. En de nieuwe lijst vervolgens toe te passen in de
functie ex1. Deze telt alle bij elkaar op. Zo heb je het inwendig product van twee lijsten.
-}

ex7 :: [Int] -> [Int] -> Int
ex7 (lst1) (lst2) = ex1( ex6 lst1 lst2)