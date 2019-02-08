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

            

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ 0 (Single m a) = Just a
indexJ i (Append m jl1 jl2) | i <= (getSize (size m)) = if i <= (getSize (size (tag jl1)))
                                        then indexJ i jl1
                                        else indexJ (i - (getSize (size (tag jl1)))) jl2
indexJ _ _ = Nothing    


--dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a             
