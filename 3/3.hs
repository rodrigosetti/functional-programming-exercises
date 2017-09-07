
setHead :: [a] -> a -> [a]
setHead [] x = [x]
setHead (_:ys) x = x:ys

main :: IO ()
main = putStrLn $ "_askell" `setHead` 'H'
