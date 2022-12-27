{-# LANGUAGE TypeApplications #-}

module Types where

import Data.Int (Int32)

type Pulse = [Float]
type Seconds = Float
type Samples = Float
type Hz = Float
type Semitones = Float
type Beats = Float
type Ringtone = String

data Tone = C | CSharp | D | DSharp | E | F | FSharp | G | GSharp | A | ASharp | B deriving (Enum, Eq, Show)
data Octave = Zero | One | Two | Three | Four | Five | Six | Seven | Eight deriving (Enum, Eq, Show)
data Duration = Full | Half | Quarter | Eighth | Sixteenth | Thirtysecond | Dotted Duration deriving (Eq, Show)
data Note = Pause Duration | Note Tone Octave Duration deriving (Eq, Show)

-- TODO: Schrijf en documenteer zipWithL, die zo ver mogelijk zipt;
-- als na het zippen, nog waarden in de eerste lijst zitten, plakt het deze er achteraan;
-- als na het zippen, nog waarden in de tweede lijst zitten, worden die weggegooid.

{-- | Deze functie past een zipWith toe op twee lijsten. Hij past een functie (paarsgewijs) toe op de elementen
van de twee lijsten. De functie zipt naar links. Als de tweede lijst leeg is, dan is het zippen klaar.
(Zit er nog wat in de eerste lijst, dan wordt dat aan de return lijst geplakt)


--}

zipWithL :: (a -> b -> a) -> [a] -> [b] -> [a]
zipWithL _ [] _= []
zipWithL _ a [] = a
zipWithL f (x:xs) (y:ys) = f x y : zipWithL f xs ys

-- TODO: Schrijf en documenteer zipWithR, die zo ver mogelijk zipt;
-- als na het zippen, nog waarden in de eerste lijst zitten, worden die weggegooid;
-- als na het zippen, nog waarden in de tweede lijst zitten, plakt het deze er achteraan.

{-- | Deze functie past een zipWith toe op twee lijsten. Hij past een functie (paarsgewijs) toe op de elementen
van de twee lijsten. Deze functie zipt naar rechts, dus als de eerste lijst leeg is, dan is het zippen klaar.
(Zit er nog wat in de tweede lijst dan wordt dat aan de uitkomst lijst geplakt)
--}

zipWithR :: (a -> b -> b) -> [a] -> [b] -> [b]
zipWithR _ _ []= []
zipWithR _ [] a = a
zipWithR f (x:xs) (y:ys) = f x y : zipWithR f xs ys 

data Sound = FloatFrames [Float]
  deriving Show

floatSound :: [Float] -> Sound
floatSound = FloatFrames

instance Eq Sound where
  (FloatFrames xs) == (FloatFrames ys) = all ((<  0.001) . abs) $ zipWith (-) xs ys

-- TODO: Schrijf de instance-declaraties van Semigroup en Monoid voor Sound.
-- Semigroup is een typeclass met een operator (<>), die twee waarden combineert;
-- in deze context betekent dat "twee geluiden na elkaar afspelen".
-- Monoid bouwt voort op Semigroup, maar heeft een mempty; een lege waarde.
instance Semigroup Sound where
  (FloatFrames a) <> (FloatFrames b) = undefined

instance Monoid Sound where
  mempty = undefined

-- TODO: Schrijf en documenteer de operator `(<+>)` die twee `Sound`s  tot een enkel `Sound` combineert.
-- Combineren betekent hier: de geluiden tegelijkertijd afspelen. 
-- Als de lijsten niet even lang zijn, moet wat er overblijft achteraan worden toegevoegd!
(<+>) :: Sound -> Sound -> Sound
(FloatFrames x) <+> (FloatFrames y) = undefined

floatToInt32 :: Float -> Int32
floatToInt32 x = fromIntegral $ round x

getAsInts :: Sound -> [Int32]
getAsInts (FloatFrames fs) = map (floatToInt32 . \x -> x * fromIntegral (div (maxBound @Int32 ) 2 )) fs

type Track = (Instrument, [Note])

newtype Instrument = Instrument (Hz -> Seconds -> Pulse)

instrument :: (Hz -> Seconds -> Pulse) -> Instrument
instrument = Instrument

newtype Modifier = Modifier (Pulse -> Pulse)

modifier :: (Pulse -> Pulse) -> Modifier
modifier = Modifier

instance Semigroup Modifier where
  (Modifier m1) <> (Modifier m2) = Modifier $ m1 . m2

-- TODO: Schrijf en documenteer de functie modifyInstrument, die een Modifier met een Instrument combineert. 
-- TIPS: Kijk goed naar de types! Gebruik een lambda om een functie te maken, die je verpakt in een Instrument.

{-- | In deze functie wordt een modifier met een instrument gecombineerd. Dit doen we mbv een lambda. Hier zijn x en y de anonieme
variabelen en wordt f x y toegepast op mod.

--}
modifyInstrument :: Instrument -> Modifier -> Instrument
modifyInstrument (Instrument f) (Modifier mod) = Instrument (\ x y -> mod $ f x y)

-- TODO: Schrijf en documenteer de functie arrange die de functie in het meegegeven Instrument toepast op de frequentie en duur. 
-- TIPS: Kijk goed naar de types!

{-- | In deze functie wordt het instrument toepast op de frequentie en de duur, hier komt de sound uit. Floatsound gebruiken we
om de hertz en seconden als een float aan sound te geven.

--}

arrange :: Instrument -> Hz -> Seconds -> Sound
arrange (Instrument f) hz seconds = floatSound (f hz seconds)
