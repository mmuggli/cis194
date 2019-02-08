fib :: Integer -> Integer
fib i = if i < 2
           then i
           else (fib (i - 1) + (fib (i - 2)))
    
fibs1 :: [Integer]
fibs1 = fmap fib [0..]


fibs2 :: [Integer]
      
fibs2 = let preds = (zip fibs2  (tail fibs2)) :: [(Integer, Integer)]
            rest = ( fmap (uncurry (+)) preds ) :: [Integer]
        in
                [0,1] ++ rest 
      
data Stream a = Cons a (Stream a)
              
streamToList :: Stream a -> [a]
streamToList (Cons a s) = a : streamToList s
                          
instance Show a => Show (Stream a) where
    show a = show $ take 20 $ streamToList a

streamRepeat :: a -> Stream a
streamRepeat a = Cons a $ streamRepeat a

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a s) = Cons (f a) (streamMap f s)
                         
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f s = Cons s $ streamFromSeed f (f s)

nats :: Stream Integer
nats = streamFromSeed (1+) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
-- this version made ruler hang. maybe play with lazy patterns here?                     
-- interleaveStreams (Cons a s) (Cons b t) = Cons a (Cons b (interleaveStreams s t))
interleaveStreams (Cons a s) t = Cons a (interleaveStreams t s)
                                 
ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamMap (1 +) ruler)
         
