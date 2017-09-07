
init' :: [a] -> [a]
init' [] = []
init' [x, _] = [x]
init' (x:xs) = x : init' xs

main :: IO ()
main = putStrLn $ init' ['a'..'z']
