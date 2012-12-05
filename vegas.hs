import Data.Array

data Suit = Club | Diamond | Heart | Spade deriving (Bounded, Enum, Show)

data Value = Two | Three | Four | Five | Six  | Seven
                 | Eight | Nine | Ten  | Jack | Queen
                 | King  | Ace deriving (Eq, Ord, Bounded, Enum, Show)

type Card = array (1)
type Deck = [Card]

deck :: Deck
deck = [(suit, value) | suit <- [minBound .. maxBound], value <- [minBound .. maxBound]]

value :: Card -> Value
value (_, v) = v

-- less clear solution using Data.Array range((0,0), (4,12)) for a deck then O(1) read time

-- fast sort shuffle(?) pick 1-max.. take use it and swap it to the max pos... decrease max and repeat
-- 

-- Lazily shuffle the same deck.. after each iteration the deck should get shuffled
