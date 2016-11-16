--21sumaPares.hs

import Data.List (foldl')

--functions
--suma pares de los cuadrados de una lista
squareEvenSum :: Integral a => [a] -> a
squareEvenSum = suma . pares . cuadrados

--suma pares de una lista de números
evenSum :: Integral a => [a] -> a
evenSum = suma . pares

cuadrados :: Integral a => [a] -> [a]
cuadrados = map (^2)

suma :: Integral a => [a] -> a
suma = foldl' (+) 0

pares :: Integral a => [a] -> [a]
pares =  filter even
