--36sumaLista.hs

--Utiliza notación bind, then y return

import Data.Maybe

--pure functions
getListFromString :: String -> Maybe [Integer]
getListFromString txt = maybeRead $ "[" ++ txt ++ "]"

maybeRead :: Read a => String -> Maybe a
maybeRead texto = case reads texto of
                    [(x, "")] -> Just x
                    _         -> Nothing

--impure functions
askUser :: IO [Integer]
askUser =
  putStrLn "Entre números separados por coma:" >>
  getLine >>= \input ->
  let maybeList = getListFromString input in
    case maybeList of
      Just lista -> return lista
      Nothing    -> askUser

main :: IO ()
main = askUser >>=
  \listado -> print . sum $ listado
