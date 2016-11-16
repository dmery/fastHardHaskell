--09absoluto.hs

-- utilizando if-then-else
absoluto :: (Ord a, Num a) => a -> a
absoluto x = if x >=0 then x else -x

-- utilizando guardas
absoluto' :: (Ord a, Num a) => a -> a
absoluto' x
  | x >= 0    = x
  | otherwise = -x
