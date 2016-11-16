--04operacion.hs

--pure function
foo :: Int -> Int -> Int
foo x y = x * x + y * y

--impure function
main = print $ foo 2 3
