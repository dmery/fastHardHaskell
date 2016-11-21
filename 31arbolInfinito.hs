--31arbolInfinito.hs

-- datatype-----------------------------------------------------
data BinTree a = Empty
               | Node a (BinTree a) (BinTree a)
               deriving (Eq, Ord)

-- instancia de BintTree a la clase de tipo Show----------------
instance (Show a) => Show (BinTree a) where
  show t = "< " ++ replace '\n' "\n: " (treeshow "" t)
    where
      treeshow pref Empty = ""
      treeshow pref (Node x Empty Empty) = (pshow pref x)


      treeshow pref (Node x left Empty) =
        (pshow pref x) ++ "\n" ++
          (showSon pref "*##" "   " left)


      treeshow pref (Node x Empty right) =
        (pshow pref x) ++ "\n" ++
          (showSon pref "*##" "   " right)


      treeshow pref (Node x left right) =
        (pshow pref x) ++ "\n" ++
          (showSon pref "|##" "    " left) ++ "\n" ++
            (showSon pref "*##" "   " right)


      showSon pref before next t =
        pref ++ before ++ treeshow (pref ++ next) t


      pshow pref x = replace '\n' ("\n" ++ pref) (show x)


      replace c new string =
        concatMap (change c new) string
        where
          change c new x
              | x == c = new
              | otherwise = x:[]

-- functions----------------------------------------------------

--crea un árbol infinito
treeMap :: (a -> b) -> BinTree a -> BinTree b
treeMap f Empty = Empty
treeMap f (Node x left right) =
  Node (f x)
    (treeMap f left)
    (treeMap f right)

infTreeTwo :: BinTree Int
infTreeTwo = Node 0 (treeMap (\x -> x-1) infTreeTwo)
                    (treeMap (\x -> x+1) infTreeTwo)

--toma datos del arbol infinito hasta
--donde indica el valor de <n>
treeTakeDepth :: (Num a1, Eq a1) => a1 -> BinTree a -> BinTree a
treeTakeDepth _ Empty = Empty
treeTakeDepth 0 _     = Empty
treeTakeDepth n (Node x left right) =
  let
    nl = treeTakeDepth (n - 1) left
    nr = treeTakeDepth (n - 1) right
  in
    Node x nl nr

-- main program-------------------------------------------------
main = print . treeTakeDepth 4 $ infTreeTwo
