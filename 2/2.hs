
isSorted :: [a] -> (a -> a -> Bool) -> Bool
isSorted l cmp = and $ zipWith cmp l (tail l)

main :: IO ()
main = do print $ isSorted [1..100] (<=)
          print $ isSorted [7,38,2,1,0,99] (<=)
          print $ isSorted [7,38,2,1,0,99] (const $ const True)
          print $ isSorted [100,99..1] (<=)
          print $ isSorted [100,99..1] (>=)
          print $ isSorted ['a'..'z'] (<=)
