--32io.hs

toList :: String -> [Integer]
toList input = read ("[" ++ input ++ "]")

main = do
    putStrLn "Entre números, separados por coma:"
    input <- getLine
    print $ sum . toList $ input
