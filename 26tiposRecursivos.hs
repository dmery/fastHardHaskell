--26tiposRecursivos.hs

--creando un contructor infijo
infix 5 :::

--datatypes
data Lista a = Nil | a ::: (Lista a)
    deriving (Show, Read, Eq, Ord)

data List a = Empty | Cons a (List a)
    deriving (Show, Read, Eq, Ord)

--function
convierteLista []     = Nil
convierteLista (x:xs) = x ::: convierteLista xs

--main program
main = do
    print (0 ::: (1 ::: Nil) )
    print (convierteLista [0,1])
    print (Cons 0 (Cons 1 Empty) )
