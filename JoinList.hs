{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

import Sized
import Scrabble
import Buffer
    
data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                    deriving (Eq, Show)

       
tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m a) = m
tag (Append m a b) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (mappend (tag a) (tag b)) a b

-- given by assignedment
-- TODO try this and the expected identities in assignment with quickcheck
jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- given by assignedment                                        
(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)                            


indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ 0 (Single m a) = Just a
indexJ i (Append m jl1 jl2) | i <= (getSize (size m)) = if i < (getSize (size (tag jl1)))
                                        then indexJ i jl1
                                        else indexJ (i - (getSize (size (tag jl1)))) jl2
indexJ _ _ = Nothing    


dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ 0 j = j
dropJ i jl | i >= getSize (size (tag jl)) = Empty
-- now we can assume that it must be an Append
-- because it's greater than 0 and less than the size
-- (assuming the metadata/cache is consistent)
dropJ i (Append m jl1 jl2) = let jl1size = (getSize (size (tag jl1))) in
                             if i < jl1size
                             then (+++) (dropJ i jl1) jl2
                             else dropJ (i - jl1size) jl2

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ 0 j = Empty
takeJ i jl | i >= getSize (size (tag jl)) = jl
takeJ i (Append m jl1 jl2) = let jl1size = (getSize (size (tag jl1))) in
                             if i <= jl1size
                             then takeJ i jl1
                             else (+++) jl1 (takeJ (i - jl1size) jl2)
                                  

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s
              



instance Buffer (JoinList (Score, Size) String) where
    toString Empty = ""
    toString (Single _ b) = b
    toString (Append _ a b) = toString a ++ toString b
    fromString s = Single (scoreString s, Size 1) s
    line i b = indexJ i b
    replaceLine i s b = let prefix = takeJ (i - 1) b
                            suffix = dropJ (i + 1) b
                            replacement = Single (scoreString s, Size 1) s in
                        prefix +++ replacement +++ suffix
    -- numLines b = case tag b of
    --                 (_, Just i) -> getSize $ size i
    --                 otherwise -> 0
    numLines Empty = 0
    numLines (Single (sc, sz) _) = getSize sz
    numLines (Append (sc, sz) _ _) = getSize sz
                                 
    value Empty = 0
    value (Single (sc, sz) _) = getScore sc
    value (Append (sc, sz) _ _) = getScore sc
