import Sized

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


-- FIXME Likely fails intended use if Empty is a leaf node
-- though specification is vague.  Likely better to use
-- right subtree of Append if the size of the left is 0
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ 0 (Single m a) = Just a
indexJ i (Append m jl1 jl2) | i <= (getSize (size m)) = if i <= (getSize (size (tag jl1)))
                                        then indexJ i jl1
                                        else indexJ (i - (getSize (size (tag jl1)))) jl2
indexJ _ _ = Nothing    


--dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a             
