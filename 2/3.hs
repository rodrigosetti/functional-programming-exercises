
-- In Haskell, all functions are already curried :)
-- so "curry" is a no-op (identity)
curry' :: (a -> b -> c) -> a -> b -> c
curry' = id

-- But... maybe you're insterested in the curry
-- version that transforms a function which domain (arity 1)
-- is a tuple into a binary function (arity 2)
curry'' :: ((a, b) -> c) -> a -> b -> c
curry'' f x y = f (x, y)

main :: IO ()
main = do let plus = curry' (+)
          print $ 2 `plus` 2
          let tp (x, y) = x + y
              plus' = curry'' tp
          print $ 2 `plus'` 2
