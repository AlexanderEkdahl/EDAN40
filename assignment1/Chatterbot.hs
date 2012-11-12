module Chatterbot where
import Utilities
import Pattern
import Random
import Char
import Random

chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
    botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine

      answer <- stateOfMind brain
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)

      if (not.endOfDialog) question then botloop else return ()

--------------------------------------------------------

type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]

--------------------------------------------------------

stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
stateOfMind b = do
  r <- randomIO :: IO Float
  let f x = (fst x, pick r (snd x))
  return (rulesApply (map f b))

-- mmap (unwords . reflect . words) (match '*' "I need *" "I need you to kill yourself")
-- rulesApply [("I need *", "Why do you need *?")] "I need you to kill yourself"
-- rulesApply [((words "I need *"), (words "Why do you need * ?"))] (words "I need you to kill yourself")
rulesApply :: [PhrasePair] -> Phrase -> Phrase
rulesApply p = try (transformationsApply "*" reflect p)

-- reflect ["i", "will", "never", "see", "my", "reflection", "in", "your", "eyes"]
reflect :: Phrase -> Phrase
reflect [] = []
reflect (p:ps) = try (flip lookup reflections) p : reflect ps

reflections =
  [ ("am",      "are"),
    ("was",     "were"),
    ("i",       "you"),
    ("i'm",     "you are"),
    ("i'd",     "you would"),
    ("i've",    "you have"),
    ("i'll",    "you will"),
    ("my",      "your"),
    ("me",      "you"),
    ("are",     "am"),
    ("you're",  "i am"),
    ("you've",  "i have"),
    ("you'll",  "i will"),
    ("your",    "my"),
    ("yours",   "mine"),
    ("you",     "me"),
    ("yourself","myself"),
    ("myself",  "yourself")
  ]

---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (=="quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|")

rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile r = (map.map2) (f, map f) r
  where f = words . map toLower

--------------------------------------

reductions :: [PhrasePair]
reductions = (map.map2) (words, words)
  [ ( "please *", "*" ),
    ( "could you *", "*" ),
    ( "can you *", "*"),
    ( "tell me if you are *", "are you *" ),
    ( "tell me who * is", "who is *" ),
    ( "tell me what * is", "what is *" ),
    ( "do you know who * is", "who is *" ),
    ( "do you know what * is", "what is *" ),
    ( "are you very *", "are you *" ),
    ( "i am very *", "i am *" ),
    ( "hi *", "hello *")
  ]

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

reductionsApply :: [PhrasePair] -> Phrase -> Phrase
reductionsApply r = fix (try (transformationsApply "*" id reductions))
