--08operacion.hs

--pure function
operacion :: Num a => a -> a -> a
operacion x y = x*x + y*y

--main program
main = do
  putStrLn "ingresa un numero:"
  x <- getLine
  putStrLn "ingresa otro numero:"
  y <- getLine
  print $ operacion (read x) (read y)
