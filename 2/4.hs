
-- In Haskell, all functions are already curried :)
-- so "uncurry" is a no-op (identity), but maybe you're intersted
-- in the uncurry version that transforms a binary function (arity 2)
-- into a function which domain is a tuple (arity 1).
uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (x, y) = f x y

main :: IO ()
main = let plus = uncurry' (+)
       in print $ plus (2, 2)
