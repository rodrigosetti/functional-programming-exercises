
data Tree a = Leaf a | Branch (Tree a) (Tree a)
      deriving Show

size :: Tree a -> Int
size (Leaf _) = 1
size (Branch l r) = size l + size r

maximum' :: Ord a => Tree a -> a
maximum' (Leaf x) = x
maximum' (Branch l r) = max (maximum' l) (maximum' r)

depth :: Tree a -> Int
depth (Leaf _) = 1
depth (Branch l r) = max (depth l) (depth r)

map' :: (a -> b) -> Tree a -> Tree b
map' f (Leaf x) = Leaf (f x)
map' f (Branch l r) = Branch (map' f l) (map' f r)

--------------------------------------------------------------------------------

main :: IO ()
main = do let t = Branch (Branch (Branch (Leaf 10) (Leaf 20)) (Leaf 7))
                         (Branch (Leaf 70) (Branch (Leaf 100) (Branch (Leaf 0) (Leaf 1))))
          putStrLn $ "size: " ++ show (size t)
          putStrLn $ "depth: " ++ show (depth t)
          putStrLn $ "maximum: " ++ show (maximum' t)
          putStrLn $ "double: " ++ show (map' (*2) t)
