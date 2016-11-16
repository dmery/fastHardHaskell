--12sumaPares.hs

evenSum :: Integral a => [a] -> a
evenSum lst = accumSum 0 lst
  where
    accumSum n [] = n
    accumSum n (x:xs) =
      if even x
        then accumSum (n + x) xs
      else accumSum n xs
