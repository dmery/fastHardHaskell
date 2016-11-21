--34error.hs

--functions
maybeRead :: Read a => String -> Maybe a
maybeRead txt = case reads txt of
                  [(x,"")]    -> Just x
                  _           -> Nothing

getListFromString :: String -> Maybe [Integer]
getListFromString str = maybeRead $ "[" ++ str ++ "]"

askUser :: IO [Integer]
askUser = do
  putStrLn "Entre números separados por coma:"
  input <- getLine
  let maybeList = getListFromString input in
    case maybeList of
      Just lista  -> return lista
      Nothing     -> askUser

--main program
main :: IO ()
main = do
  list <- askUser
  print . sum $ list
