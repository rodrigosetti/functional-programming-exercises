
drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' n (x:xs)
  | n <= 0 = x:xs
  | otherwise = drop' (n-1) xs

main :: IO ()
main = putStrLn $ drop' 10 ['a'..'z']
