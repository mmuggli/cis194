{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Scrabble where

import Data.Monoid
import Data.Map (Map)             -- This just imports the type name
import qualified Data.Map as Map  -- Imports everything else, but with names 
                                  -- prefixed with "Map." (with the period).
    
newtype Score = Score Int
    deriving (Eq, Ord, Show, Num)
             
instance Monoid Score where
    mempty = Score 0
    mappend = (+)

instance Semigroup Score where
    Score a <> Score b = Score (a + b)

-- ore function should implement the tile scoring values as
-- http://www.thepixiepit.co.uk/scrabble/rules.html;

scoremap :: Map Char Int
scoremap = Map.fromList [('a', 1), ('b', 3), ('c', 3), ('d', 2), ('e', 1), ('f', 4), ('g', 2), ('h', 4), ('i', 1), ('j', 8), ('k', 5), ('l', 1), ('m', 3), ('n', 1), ('o', 1), ('p', 3), ('q', 10), ('r', 1), ('s', 1), ('t', 1), ('u', 1), ('v', 4), ('w', 4), ('x', 8), ('y', 4), ('z', 10)]

  
                      
score :: Char -> Score
score c = case scoremap Map.!? c of
            Just i -> Score i
            Nothing -> Score 0
         
         
--scoreString :: String -> Score    

