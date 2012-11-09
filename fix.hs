lol :: Int -> Int
lol 1 = 4
lol 2 = 2
lol 3 = 34
lol 4 = 4

square :: Num a => a -> a
square x = x^2

fix :: Eq a => (a -> a) -> a -> a
fix f x
   |  f x == x  = x
   |  otherwise = fix f (f x)

--main = print (fix square 1)
