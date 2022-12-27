module Parsers where

import Control.Monad.Trans.State (StateT(..), evalStateT, put, get)
import Control.Applicative
import Control.Monad.Trans.State (StateT(..), evalStateT, put, get)
import Data.Maybe (isJust, fromMaybe)
import Data.Char (toUpper)
import Control.Monad(mzero, mplus)
import Data.List (uncons)
import Types
import Instruments

type Parser = (StateT String Maybe)
type CharSet = [Char]

pCharSet :: CharSet -> Parser Char
pCharSet cs = do input <- uncons <$> get
                 case input of
                   Just (h, t) -> if h `elem` cs then put t >> return h else mzero
                   Nothing -> mzero

-- TODO: Schrijf en documenteer de Parser `pComplementCharSet` die een lijst van karakters meekrijgt,
-- en het eerste karakter parset wanneer dit niet in de meegegeven set karakters zit.

{-- | Deze functie parset een karakter uit wanneer hij níét in de list voor komt, daarom gebruik je 
de functie pCharset met een kleine aanpassing. Met notElem wordt er gekeken of het karakter niet in de lijst voor komt.
--}
pComplementCharSet :: CharSet -> Parser Char
pComplementCharSet cs = do input <- uncons <$> get
                           case input of
                             Just (h, t) -> if h `notElem` cs 
                                            then put t >> return h 
                                            else mzero
                             Nothing -> mzero


-- TODO: Schrijf en documenteer de Parser `pString` die een gegeven String probeert te parsen. 
-- TIPS: Parse een enkele letter met `pCharSet` en parse de rest recursief met `pString`; 
--       combineer beide waarden weer voordat je deze returnt. 
--       Vergeet niet een geval voor een lege String te schrijven.

{-- | Deze functie parset een string, dit doet hij met behulp van de  functie pCharSet op recursieve manier.
Hij maakt eerst een pCharSet van van het eerste item. Daarna doet hij pString op de rest, en roept hij de
functie opnieuw aan.
--}

pString :: String -> Parser String
pString [] = return mzero
pString (x:xs) = do x <- pCharSet [x]
                    xs <- pString xs
                    return $ x : xs
	
pOptional :: Parser a -> Parser (Maybe a)
pOptional p = Just <$> p <|> return Nothing 

pRepeatSepBy :: Parser a -> Parser b -> Parser [b]
pRepeatSepBy sep p = (:) <$> p <*> mplus (sep *> pRepeatSepBy sep p) (return [])

pEmpty :: Parser ()
pEmpty = return ()

-- TODO: Schrijf en documenteer de Parser `pRepeat` die een enkele Parser herhaaldelijk toepast.
-- TIPS: Maak gebruik van `pRepeatSepBy` en `pEmpty`.

{-- | Deze functie gebruikt een parser steeds opnieuw, dit doet hij met behulp van de pEmpty en pRepeatSepBy.
Deze past hij toe op variabel a.
--}

pRepeat :: Parser a -> Parser [a]
pRepeat a = pEmpty `pRepeatSepBy` a 


nums :: CharSet
nums = "12345678"

-- TODO: Schrijf en documenteer de Parser `pNumber`, die een geheel getal parset.
-- TIPS: Combineer de voorgaande Parsers en de Prelude-functie read. 

{-- | Deze functie kan hele getallen parsen (1 t/m 8). er wordt van te voren al een charset
met nummers gemaakt, waar vervolgens overheen wordt geparset met de functie pCharset
(en ook met read en pRepeat)
--}

pNumber :: Parser Int
pNumber = fmap (read) (pRepeat $ pCharSet nums)

pTone :: Parser Tone
pTone = do tone <- tRead . toUpper <$> pCharSet "abcdefg"
           sharp <- pOptional (pCharSet "#")
           if isJust sharp && tone `elem` [C,D,F,G,A]
             then return (succ tone)
             else return tone
  where tRead 'C' = C
        tRead 'D' = D
        tRead 'E' = E
        tRead 'F' = F
        tRead 'G' = G
        tRead 'A' = A
        tRead 'B' = B
        tRead _   = error "Invalid note"

-- TODO: Schrijf en documenteer de Parser` `pOctave`, die een getal parset naar de bijbehorende octaaf.
-- TIPS: Kijk in Types.hs naar het type Octave voor je begint te schrijven.

{-- | Deze functie maakt van een getal een octaaf. Een octaaf kan een getal van 1 t/m 8 zijn. (toEnum) Integer combineert met de pNumber.
In pNumber maakt al gebruik van de getallen 1 t/m 8!
--}

pOctave :: Parser Octave
pOctave = toEnum <$> pNumber

pDuration :: Parser Duration
pDuration = do number <- pNumber
               case number of
                 1 -> return Full
                 2 -> return Half
                 4 -> return Quarter
                 8 -> return Eighth
                 16 -> return Sixteenth
                 32 -> return Thirtysecond
                 _ -> mzero

pPause :: Duration -> Parser Note
pPause d = do duration <- fromMaybe d <$> pOptional pDuration
              _ <- pCharSet "pP"
              return $ Pause duration

pNote :: Duration -> Octave -> Parser Note
pNote d o = do duration <- fromMaybe d <$> pOptional pDuration
               tone <- pTone
               dot <- pOptional (pCharSet ".")
               octave <- fromMaybe o <$> pOptional pOctave
               return $ Note tone octave (if isJust dot then Dotted duration else duration)

pComma :: Parser ()
pComma = () <$ do _ <- pCharSet ","
                  pOptional (pCharSet " ")

-- TODO: Schrijf en documenteer de Parser `pHeader`, die de start van de RTTL-string parset.
-- TIPS: We hebben je de naam van het bestand, en het converteren van bpm met fromIntegral vast gegeven.
--       Het stuk dat je rest om te parsen zit tussen de twee dubbele punten!

{-- | Deze functie is een combinatie van een aantal voorgaande functies, hier wordt het begin van de RTTL
string gemaakt. Er worden 4 waardes meegegeven aan de string. Dat zijn de naam, de duur, de octaaf en de bpm.

--}

pHeader :: Parser (String, Duration, Octave, Beats)
pHeader = do name <- pRepeat (pComplementCharSet ":")
             _ <- pCharSet ":"
             _ <- pOptional (pCharSet " ")
             _ <- pString "d="
             duration <- pDuration
             _ <- pComma
             _ <- pString "o="
             octave <- pOctave
             _ <- pComma
             _ <- pString "b="
             bpm <- fromIntegral <$> pNumber
             _ <- pCharSet ":"
             _ <- pOptional (pCharSet " ")
             return (name, duration, octave, bpm)

pSeparator :: Parser ()
pSeparator = () <$ foldl1 mplus [pString " ", pString ", ", pString ","]

pRTTL :: Parser (String, [Note], Beats)
pRTTL = do (t, d, o, b) <- pHeader
           notes <- pRepeatSepBy pSeparator $ mplus (pNote d o) (pPause d)
           return (t, notes, b)

parse :: String -> Maybe (String, [Note], Beats)
parse x = evalStateT pRTTL x