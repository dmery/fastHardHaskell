--15sumaPares.hs

evenSum lst = mysum 0 (filter even lst)
  where
    mysum n []     = n
    mysum n (x:xs) = mysum (n + x) xs
