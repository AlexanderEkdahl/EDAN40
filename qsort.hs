qsort [] = []
qsort (p:xs) = qsort([a | a <- xs, a <= p]) ++ [p] ++ qsort([a | a <- xs, a > p])
