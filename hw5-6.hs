{-# LANGUAGE FlexibleInstances #-}
import qualified Data.Map as M

class HasVars a where
    var :: String -> a

data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT
           | Var String
