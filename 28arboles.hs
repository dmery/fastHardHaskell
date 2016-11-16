--28arboles.hs

-- datatype----------------------------------------------------------
data BinTree a = Empty
               | Node a (BinTree a) (BinTree a)
               deriving (Eq, Ord)

-- instancia de BintTree a la clase de tipo Show---------------------
instance (Show a) => Show (BinTree a) where
  show t = "< " ++ replace '\n' "\n: " (treeshow "" t)
    where
      treeshow pref Empty = ""
      treeshow pref (Node x Empty Empty) = (pshow pref x)

      treeshow pref (Node x left Empty) =
        (pshow pref x) ++ "\n" ++
        (showSon pref "*--" "  " left)

      treeshow pref (Node x Empty right) =
        (pshow pref x) ++ "\n" ++
        (showSon pref "*--" "   " right)

      treeshow pref (Node x left right) =
        (pshow pref x) ++ "\n" ++
        (showSon pref "|--" "   " left) ++ "\n" ++
        (showSon pref "*--" "   " right)

      showSon pref before next t =
        pref ++ before ++ treeshow (pref ++ next) t

      pshow pref x = replace '\n' ("\n" ++ pref) (show x)

      replace c new string =
        concatMap (change c new) string
        where
          change c new x
              | x == c = new
              | otherwise = x:[] -- "x"

-- functions---------------------------------------------------------

--convierte una lista en árbol binario
treeFromList :: Ord a => [a] -> BinTree a
treeFromList [] = Empty
treeFromList (x:xs) = Node x (treeFromList (filter (<x) xs) )
                             (treeFromList (filter (>x) xs) )

--arbol infinito
nullTree :: BinTree Integer
nullTree = Node 0 nullTree nullTree

treeTakeDepth :: (Eq a1, Num a1) => a1 -> BinTree a -> BinTree a
treeTakeDepth _ Empty = Empty
treeTakeDepth 0 _     = Empty
treeTakeDepth n (Node x left right) =
    let
      nl = treeTakeDepth (n-1) left
      nr = treeTakeDepth (n-1) right
    in
      Node x nl nr

treeMap :: (a -> b) -> BinTree a -> BinTree b
treeMap f Empty = Empty
treeMap f (Node x left right) = Node (f x)
    (treeMap f left)
    (treeMap f right)

infTreeTwo :: BinTree Int
infTreeTwo = Node 0 (treeMap (\x -> x - 1) infTreeTwo)
                    (treeMap (\x -> x + 1) infTreeTwo)

-- programa principal------------------------------------------------
main = do
    putStrLn "Arbol binario de enteros:"
    print . treeFromList $ [7,2,4,8,1,3,6,21,12,23]
    putStrLn "************************************"
    putStrLn "Arbol binario de cadenas:"
    print . treeFromList $ ["foo","bar", "baz", "gor","yog"]
    putStrLn "************************************"
    putStrLn "Arbol binario de caracteres:"
    print . treeFromList . map treeFromList $ ["baz","zara","bar"]
    putStrLn "************************************"
    putStrLn "Arbol binario de 4 niveles con ceros"
    print . treeTakeDepth 4 $ nullTree
    putStrLn "************************************"
    putStrLn "Arbol con incrementos y decrementos"
    print . treeTakeDepth 4 $ infTreeTwo
