--30arbolInfinito.hs

-- datatype------------------------------------------------
data BinTree a = Empty
               | Node a (BinTree a) (BinTree a)
               deriving (Show, Eq, Ord)

-- functions-----------------------------------------------

--crea un arbol infinito
nullTree :: BinTree Integer
nullTree = Node 0 nullTree nullTree

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

-- main program--------------------------------------------
main = print . treeTakeDepth 4 $ nullTree
