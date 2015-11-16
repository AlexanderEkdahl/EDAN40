module Utilities where

-- Takes 2 tuples. One holding 2 functions and the other tuple holding 2 valid
-- arguments for the 2 functions in the previous tuple. Outputs the results of
-- the two functions running on the arguments in the second tuple.
map2 :: (a -> b, c -> d) -> (a, c) -> (b, d)
map2 (f1, f2) (x1, x2) = (f1 x1, f2 x2)

-- If the second argument is unequal to Nothing the function f is run on value of the second argument
mmap :: (a -> b) -> Maybe a -> Maybe b
mmap = fmap

-- Returns the second argument if the first argument is Nothing, otherwise it returns the first argument
orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing x  = x
orElse (Just a) _ = Just a

-- Tries running the function f on parameter x. If it is unequal to Nothing it returns the value of the function.
-- Otherwise returns the default value of x
try :: (a -> Maybe a) -> a -> a
try f x = maybe x id (f x)

-- Recursively runs the result of the function untill the parameter matches the result of the function
-- Returns the matching x
fix :: Eq a => (a -> a) -> a -> a
fix f x
   |  f x == x  = x
   |  otherwise = fix f (f x)

-- Picks an element from xs depending on u being from 0.0(the first element) to 1.0 the last element
pick :: RealFrac r => r -> [a] -> a
pick u xs = xs !! (floor.(u*).fromIntegral.length) xs
