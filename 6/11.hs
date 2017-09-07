import Control.Monad
import Control.Applicative

newtype State s a = State { run :: s -> (a, s) }

instance Functor (State s) where

  fmap f (State g) = State $ \s -> let (a, s') = g s in (f a, s')

instance Applicative (State s) where

  pure a = State $ \s -> (a, s)

  (State f) <*> (State g) = State $ \s -> let (h, _) = f s
                                              (a, s') = g s
                                            in (h a, s')

instance Monad (State s) where

  (State r) >>= f = State $ \s -> let (a, s') = r s
                                      (State r') = f a
                                   in r' s'

get :: State s s
get = State $ \s -> (s, s)

set :: s -> State s ()
set s = State $ const ((), s)

modify :: (s -> s) -> State s ()
modify f = f <$> get >>= (set . f)

runState :: State s a -> s -> a
runState m = fst . run m

--------------------------------------------------------------------------------

data Input = Coin | Turn

type Candies = Int
type Coins = Int

data Machine = Machine { locked :: Bool, candies :: Candies, coins :: Coins }

lock :: Machine -> Machine
lock m = m { locked = True }

unlock :: Machine -> Machine
unlock m = m { locked = False }

dispenseCandy :: Machine -> Machine
dispenseCandy m = m { candies = candies m - 1 }

addCoin :: Machine -> Machine
addCoin m = m { coins = coins m + 1 }

simulateMachine :: [Input] -> State Machine (Candies, Coins)
simulateMachine inputs =
  do forM_ inputs operation
     liftA2 (,) (coins <$> get) (candies <$> get)
  where
    operation op = do ca <- candies <$> get
                      when (ca > 0) $
                        case op of
                          Coin ->
                            do modify unlock
                               modify addCoin
                          Turn ->
                            do isLocked <- locked <$> get
                               unless isLocked $ do modify lock
                                                    modify dispenseCandy

--------------------------------------------------------------------------------

main :: IO ()
main = let simulation = simulateMachine [Coin, Turn, Coin, Turn, Turn, Coin]

        in print $ runState simulation $ Machine False 10 8
