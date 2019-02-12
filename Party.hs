module Party where

import Employee
import Data.Tree
    
glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp { empFun = ef}) (GL es esf) = GL (e:es) (ef + esf)

    
instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL es f) (GL fs g) = GL (es ++ fs) (f + g)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2 | compare gl1 gl2 == GT = gl1
moreFun gl1 gl2 = gl2



-- Data.Tree
-- a is going to be bound to an Employee
-- b is going to be bound to (GuestList, GuestList)
-- function is going to be (Employee -> [(GuestList, Guestlist)] -> (GuestList, GuestList) == ( a -> [b] -> b)
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f  (Node {rootLabel = rl, subForest = sf}) = f rl (fmap (treeFold f) sf)
            
-- treeFold :: (b -> a -> b) -> b -> Tree a -> b
-- treeFold f init (Node {rootLabel = rl, subForest = []}) = (f init rl)
-- treeFold f init (Node {rootLabel = rl, subForest = (st : sts)}) = treeFold f (treeFold f init st) (Node rl sts)


nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
--nextLevel e@(Emp {empFun = f}) [] = ((GL [e] f), (GL [] 0))
nextLevel e@(Emp {empFun = f}) gl = let with = glCons e $ mconcat $  map snd gl
                                        without = mconcat $ map fst gl in
                                    (with, without)
--nextLevel 

--treeFold :: () 
             
maxFun :: Tree Employee -> GuestList
maxFun t =  case treeFold nextLevel  t of
             (gl1, gl2) -> moreFun gl1 gl2
