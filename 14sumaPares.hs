--14sumaPares.hs

evenSum :: Integral a => [a] -> a
evenSum = accumSum 0
  where
    accumSum n []     = n
    accumSum n (x:xs)
      | even x    = accumSum (n + x) xs
      | otherwise = accumSum n xs
