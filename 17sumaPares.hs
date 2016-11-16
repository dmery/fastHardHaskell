--17sumaPares.hs

import Data.List (foldl')

evenSum :: Integral a => [a] -> a
evenSum lst = foldl' (\x y -> x+y) 0 (filter even lst)
