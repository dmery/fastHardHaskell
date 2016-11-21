--35sumaLista.hs

--utiliza sintáxis do

--pure functions
getListFromString :: String -> Maybe [Integer]
getListFromString cadena = maybeRead $ "[" ++ cadena ++ "]"

maybeRead :: Read a => String -> Maybe a
maybeRead string = case reads string of
                     [(x,"")] -> Just x
                     _        -> Nothing

--impure functions
askUser :: IO [Integer]
askUser = do
  putStrLn "Entra números separados por coma:"
  input <- getLine
  let maybeList = getListFromString input in
    case maybeList of
      Just lista -> return lista
      Nothing    -> askUser

main :: IO ()
main = do
  list <- askUser
  print . sum $ list
