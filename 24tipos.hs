--24tipos.hs

--datatype
data Nombre = NomConst String
data Color  = ColConst String

--function
showInfos :: Nombre ->  Color -> String
showInfos (NomConst nom) (ColConst col) =
  "Nombre: " ++ nom ++ ", Color: " ++ col

--values
nombre = NomConst "Giovanna"
color  = ColConst "azul"

--main program
main = putStrLn $ showInfos nombre color
