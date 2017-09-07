
tail' :: [a] -> [a]
tail' [] = []
tail' (_:xs) = xs

main :: IO ()
main = do print $ tail' "kdjlhafkjh"
          print $ tail' [1..100]
