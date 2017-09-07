
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f (x:xs)
  | f x = dropWhile' f xs
  | otherwise = x:xs

main :: IO ()
main = putStrLn $ dropWhile' (<= 'j') ['a'..'z']
