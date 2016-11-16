--07operacion.hs

--pure functions
f :: Num a => a -> a -> a
f x y = x * x + y * y

--impure function
main = print (f x y)

--values
x :: Int
x = 3
y :: Float
y = 2.4
