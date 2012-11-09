square :: Num a => a -> a
square x = x^2

lol 2 = 2

mmap :: (a -> b) -> Maybe a -> Maybe b
mmap f  Nothing  = Nothing
mmap f (Just x)  = Just (f x)

--main = print (mmap square (Just 2))

--add :: Integer -> Integer -> Integer
--add x y =  x + y

--main = print (add 3 4)

--kek :: (a -> b) -> Maybe a -> Maybe b
--kek f  Nothing  = Nothing
--kek f (Just x)  = Just (f x)

--main = print 3

--bur                     :: (a->b) -> [a] -> [b]
--bur f  []               =  []
--bur f (x:xs)            =  f x : map f xs

--main = print (bur square [3,4,5,3])
