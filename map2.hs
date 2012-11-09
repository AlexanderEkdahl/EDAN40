square :: Int -> Int
square x = x^2

--map2 :: (a -> b, c -> d) -> (a, c) -> (b, d)
--map2 (f1, f2) (x1, x2) = (f1 x1, f2 x2)

--main = print ( map2 (length, head) ([2,3], [2]) )

----main = print ( map2 (square, square, [1..5]) )

--main = print ( map2 (square [1,2,3,4,5], square [1,2,3,4,5]) )


--pick :: RealFrac r => r -> [a] -> a
--pick u xs = xs !! (floor.(u*).fromIntegral.length) xs

--main = print (pick (2, [3, 4, 5, 85, 3]))

--kek :: [a] -> Int
--kek [] =  0
--kek (x:xs) =  1 + length xs

main = print (map square [3, 4, 5])
