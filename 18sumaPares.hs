--18sumaPares.hs

import Data.List (foldl')

evenSum :: Integral a => [a] -> a
evenSum lst = foldl' (+) 0 $ filter even lst
