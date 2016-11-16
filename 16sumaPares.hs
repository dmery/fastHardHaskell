--16sumaPares.hs

import Data.List

evenSum :: Integral a => [a] -> a
evenSum lst = foldl' mysum 0 $ filter even lst
  where mysum acc value = acc + value
