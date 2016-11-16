--11sumaPares.hs

--function
evenSum :: Integral a => [a] -> a
evenSum lst = accumSum 0 lst
  where accumSum n lst =
          if lst == []
            then n
          else let x  = head lst
                   xs = tail lst
               in if even x
                    then accumSum (n+x) xs
                  else accumSum n xs
