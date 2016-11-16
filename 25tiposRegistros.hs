--25tiposRegistros.hs

{-# LANGUAGE DatatypeContexts #-}

--record syntax datatype
data Num a =>   Complex a = Complex
  { real :: a
  , img  :: a
  } deriving Show

--values
c = Complex 1.0 2.0
z = Complex {real = 3, img = 4}

--main program
main = putStrLn $ show c ++ " " ++ show z
