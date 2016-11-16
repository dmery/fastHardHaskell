--20sumaPares.hs

import Data.List (foldl')

evenSum :: Integral a => [a] -> a
evenSum = suma . filter even

suma :: (Num a) => [a] -> a
suma = foldl' (+) 0
