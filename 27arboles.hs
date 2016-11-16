--27arboles.hs

import Data.List

--datatype
data BinTree a = Empty
               | Node a (BinTree a) (BinTree a)
               deriving Show

--function: convierte una lista en árbol binario
treeFromList :: Ord a => [a] -> BinTree a
treeFromList [] = Empty
treeFromList (x:xs) = Node x (treeFromList . filter (<x) $ xs)
                             (treeFromList (filter (>x) xs) )

--programa principal
main = do
  putStrLn "ingresa numeros seguidos por coma"
  lista <- getLine
  print . treeFromList $ lista

{-
la forma de mostrar el árbol no es facilmente "legible"
si ingresamos [7,2,4,8,3] la función nos devuelve:
Node 7 (Node 2 Empty (Node 4 (Node 3 Empty Empty) Empty))
(Node 8 Empty Empty), por lo tanto en arboles1.hs
utilizaremos otro código
-}
