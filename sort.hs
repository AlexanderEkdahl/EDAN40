-- quite slow
quicksort [] = []
quicksort (p:xs) = quicksort([a | a <- xs, a <= p]) ++ [p] ++ quicksort([a | a <- xs, a > p])
