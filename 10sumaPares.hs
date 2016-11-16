--10sumaPares.hs

--functions
evenSum :: [Integer] -> Integer
evenSum lst = accumSum 0 lst

accumSum :: Integral t => t -> [t] -> t
accumSum n lst = if lst == []
                   then n
                 else let x  = head lst
                          xs = tail lst
                      in if even x
                           then accumSum (n + x) xs
                         else accumSum n xs
