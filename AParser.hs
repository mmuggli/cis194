{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f :: String -> Maybe (Char, String)
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------
first :: (a -> b) -> (a,c) -> (b,c)
first f (a,c) = (f a,c)

instance Functor Parser where
    fmap f fa = Parser g
         --g :: String -> Maybe (b, String) TODO: figure out why compile fails
        where g a = case (runParser fa) a of
                      Nothing -> Nothing
                      Just (c, s) -> Just (f c, s)
--                                  :: Functor f => (a -> b) -> f a -> f b

instance Applicative Parser where
    pure x = Parser g
        where g s = Just (x, s)
                    
    (<*>) p1 p2 = Parser the_parser
        where the_parser s = case (runParser p1) s of
                      Nothing -> Nothing
                      Just (a_func, rem_s) -> case (runParser p2) rem_s of
                                        Nothing -> Nothing
                                        Just (some_val, s3) -> Just (a_func some_val, s3)


abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'
           
-- this works but seems odd to make a lambda function, like the assignment
-- wants us to use some other function we are already supposed to know about
-- maybe pure?
abParser_ :: Parser ()
abParser_ = (\x y -> ()) <$> char 'a' <*> char 'b'             

intPair :: Parser [Integer]
intPair = (\x y z -> [x,z]) <$> posInt <*> char ' ' <*> posInt

instance Alternative Parser where
    empty = Parser (\s -> Nothing)

    (<|>) p1 p2 = Parser the_parser where
        the_parser s = case (runParser p1) s of
                         p1res@(Just (v, rem_s)) -> p1res
                         Nothing -> case (runParser p2) s of
                                      Nothing -> Nothing
                                      p2res@(Just (v, rem_s)) -> p2res

intOrUppercase :: Parser ()
intOrUppercase = (\x -> ()) <$> (posInt) <|> (\y -> ()) <$> (satisfy isUpper)

