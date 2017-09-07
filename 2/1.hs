import Control.Monad

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

main :: IO ()
main = forM_ [fib n | n <- [1..20]] print
