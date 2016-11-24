--42arbolInfinito.hs

-- datatype----------------------------------------------------------
data BinTree a = Empty | Node a (BinTree a) (BinTree a)
  deriving (Eq, Ord)

{-
instancia de BintTree en la clase de tipo Show
permite mostrar un árbol en forma mas gráfica
-}
instance (Show a) => Show (BinTree a) where
  show t = "< " ++ replace '\n' "\n: " (treeshow "" t)
    where
      treeshow pref Empty = ""
      treeshow pref (Node x Empty Empty) = (pshow pref x)

      treeshow pref (Node x left Empty) =
        (pshow pref x) ++ "\n" ++
        (showSon pref "*>>" "   " left)

      treeshow pref (Node x Empty right) =
        (pshow pref x) ++ "\n" ++
        (showSon pref "*>>" "    " right)

      treeshow pref (Node x left right) =
        (pshow pref x) ++ "\n" ++
        (showSon pref "|>>" "    " left) ++ "\n" ++
        (showSon pref "*>>" "    " right)

      showSon pref before next t =
        pref ++ before ++ treeshow (pref ++ next) t

      pshow pref x = replace '\n' ("\n" ++ pref) (show x)

      replace c new string =
        concatMap (change c new) string
        where
          change c new x
              | x == c = new
              | otherwise = x:[]

-- pure functions----------------------------------------------------

--pseudo random number list
shuffle :: [Integer]
shuffle = map rand [1..]
  where
    rand x = ((p x) `mod` (x+c)) - ((x+c) `div` 2)
    p x = m * x ^ 2 + n * x + o
    m = 3123
    n = 31
    o = 7641
    c = 1237

{-
crea una árbol binario a partir de una lista
-}
treeFromList :: (Ord a) => [a] -> BinTree a
treeFromList []     = Empty
treeFromList (x:xs) = Node x (treeFromList (safefilter (<x) xs))
                             (treeFromList (safefilter (>x) xs))

{-
safefilter reemplaza a la función filter
lo cual permite manejar estructures de
listas infinitas hasta 1000 pasos
-}
safefilter :: (a -> Bool) -> [a] -> [a]
safefilter f lis = safefilter' f lis nbTry
  where
    nbTry = 1000
    safefilter' _ _ 0      = []
    safefilter' _ [] _     = []
    safefilter' f (x:xs) n =
      if f x
        then x:safefilter' f xs nbTry
        else safefilter' f xs (n - 1)

{-
crea un árbol binario con una
profundidad cuyo valor expresa <n>
-}
treeTakeDepth :: (Num a1, Eq a1) => a1 -> BinTree a -> BinTree a
treeTakeDepth _ Empty = Empty
treeTakeDepth 0 _     = Empty
treeTakeDepth n (Node x left right) =
  let
    nl = treeTakeDepth (n - 1) left
    nr = treeTakeDepth (n - 1) right
  in
    Node x nl nr

-- impure function---------------------------------------------------
main = do
  putStrLn "lista de 10 números pseudo aleatorios:"
  print $ take 10 shuffle
  putStrLn "\ntreeTakeDepth 8 (treeFromList shuffle):"
  print . treeTakeDepth 8 $ treeFromList shuffle
