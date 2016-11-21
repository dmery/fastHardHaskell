--35io.hs

import System.Environment (getArgs)

--function
sumArgs :: Num a => [a] -> a
sumArgs lst = sum lst

--main program
main = do
  putStrLn "ingresa números separados por coma:"
  input <- getArgs
  print input
