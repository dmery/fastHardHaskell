--22sumaPares.hs

import Data.List (foldl')

--functions

--listas intencionales
evenSum :: Integral a => [a] -> a
evenSum xs = sum [x | x <- xs, even x]

--composición de funciones: sum, map y función lambda con mod
evenSum1 :: Integral a => [a] -> a
evenSum1 = sum . map (\x -> if x `mod` 2 == 0 then x else 0)

--utilizamos recursividad
evenSum2 :: Integral a => [a] -> a
evenSum2 = acumulaSum 0
  where
    acumulaSum n [] = n
    acumulaSum n (x:xs) =
      if even x
        then acumulaSum (n + x) xs
      else acumulaSum n xs

--composición de funciones: sum, filter y even
evenSum3 :: Integral a => [a] -> a
evenSum3 = sum . filter even

--composición de funciones: foldl', filter y even
evenSum4 :: Integral a => [a] -> a
evenSum4 = (foldl' (+) 0) . (filter even)

{-
composición de funciones: suma y pares,
ambas funciones definidas
-}
evenSum5 :: Integral a => [a] -> a
evenSum5 = suma . pares

suma :: Integral a => [a] -> a
suma = foldl' (+) 0

pares :: Integral a => [a] -> [a]
pares =  filter even
