--39banco.hs

--pure functions
deposit :: (Num a) => a -> a -> Maybe a
deposit value account = Just (account + value)

withdraw :: (Num a, Ord a) => a -> a -> Maybe a
withdraw value account = if (account < value)
  then Nothing
  else Just (account - value)

eligible :: (Num a, Ord a) => a -> Maybe Bool
eligible account =
  deposit 100 account >>=
  withdraw 200 >>=
  deposit 100  >>=
  withdraw 300 >>=
  deposit 1000 >>
  return True

--impure functions
main = (print . eligible $ 300) >>
  (print . eligible $ 299)
