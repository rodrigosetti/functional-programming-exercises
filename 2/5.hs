
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

main :: IO ()
main = let f = reverse `compose` tail
        in putStrLn $ f "haskell"
