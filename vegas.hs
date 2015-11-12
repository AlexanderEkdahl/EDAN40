module Main where

import System.Random (randomRIO)
import Data.Array.IO
import Control.Monad

data Suit = Club | Diamond | Heart | Spade
  deriving (Show, Enum)

data Value = Two | Three-- | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Show, Enum)

type Card = (Suit, Value)
type Deck = [Card]

next :: Suit -> Suit
next Spade = Club
next x = succ x

makeDeck :: Deck
makeDeck = [(suit, value) | suit <- [Club .. Spade], value <- [Two .. Three]]

shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs

sequentialDeck :: Deck -> Bool
sequentialDeck d = f d Club
  where f [] _ = True
        f (c:cs) s =
          if matchSuit c s
            then f cs (next s)
            else False

matchSuit :: Card -> Suit -> Bool
matchSuit (Club, _) Club = True
matchSuit (Diamond, _) Diamond = True
matchSuit (Heart, _) Heart = True
matchSuit (Spade, _) Spade = True
matchSuit _ _ = False

main :: IO ()
main = repeat 1
  where repeat x = fmap sequentialDeck (shuffle makeDeck) >>= (\v -> if v then putStrLn (show x) else repeat (x+1))
