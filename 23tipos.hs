 --23tipos.hs

--type alias
type Nombre = String
type Color  = String

--function
showInfos :: Nombre -> Color -> String
showInfos nom col =
  "Nombre: " ++ nom ++ ", Color: " ++ col

--values
nombre :: Nombre
nombre = "Diana"

color  :: Color
color = "rosa"

--main program
main = putStrLn $ showInfos nombre color
