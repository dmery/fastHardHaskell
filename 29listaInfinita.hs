--29listaInfinta.hs

-- functions-------------------------------------

--crea una lista números = [1,2,..]
numeros :: [Integer]
numeros = 0:map (1+) numeros

--toma una cantidad de elementos de la lista
take' :: (Num a, Eq a) => a -> [b] -> [b]
take' n []     = []
take' 0 _      = []
take' n (x:xs) = x:take'(n - 1) xs

-- main program----------------------------------
main = print . take' 10 $ numeros
