try :: (a -> Maybe a) -> a -> a
try f x = maybe x id (f x)

lol :: Maybe a -> a
lol (Just a) = a

--main = print (try lol 3)
