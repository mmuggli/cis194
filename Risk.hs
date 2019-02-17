{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Debug.Trace
import Data.List
------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

-- sequence :: Monad m => [m a] -> m [a]
-- sequence [] = return []
-- sequence (ma:mas) =
--   ma >>= \a ->
--   sequence mas >>= \as ->
--   return (a:as)


-- replicateM :: Monad m => Int -> m a -> m [a]
-- replicateM n m = sequence (replicate n m)

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
                 deriving (Show)
battle :: Battlefield -> Rand StdGen Battlefield                 
battle Battlefield { attackers = a, defenders = d} =
    do let actual_a = min (a - 1) 3
       let actual_d = min (d) 2
       a_rolls <- replicateM actual_a die
       d_rolls <- replicateM actual_d die
       let ars = reverse $ Data.List.sort a_rolls
       let drs = reverse $ Data.List.sort d_rolls
       let pairs = zip ars drs
       let awin = filter (\(ar, dr) -> ar > dr) pairs
       return (Battlefield (a - ((length pairs) - (length awin)))
                           (d - (length awin))) -- bogus
                                                                   

                                                     
main :: IO ()
main = do bf <- evalRandIO (battle (Battlefield 3 3))
          putStrLn $ show $  bf

