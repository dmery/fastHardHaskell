--33error.hs

--function
maybeRead :: Read a => String -> Maybe a
maybeRead txt = case reads txt of
                  [(x,"")]    -> Just x
                  _           -> Nothing

getListFromString :: String -> Maybe [Integer]
getListFromString str = maybeRead $ "[" ++ str ++ "]"

--main program
main :: IO ()
main = do
  putStrLn "Entre números separados por coma:"
  input <- getLine
  let maybeList = getListFromString input in
    case maybeList of
      Just lista  -> print . sum $ lista
      Nothing     -> error "formato malo. Adios."
