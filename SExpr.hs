{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser -- renamed since hw10 uses a different AParser
import Control.Applicative
import Data.Char    

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = (:) <$> p <*> zeroOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p -- undefined

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
ident = (:) <$> (satisfy isAlpha) <*> (zeroOrMore $ satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

parseInt :: Parser SExpr
parseInt =  Parser f where
    f s = case (runParser posInt) s of
             Nothing -> Nothing
             Just (i, s) -> Just (A (N i), s)

parseIdent :: Parser SExpr
parseIdent = Parser f where
    f s = case (runParser ident) s of
               Nothing -> Nothing
               Just (i, s) -> Just (A (I i), s)               
  
parseAtom :: Parser SExpr
parseAtom = parseInt <|> parseIdent

parseComb :: Parser SExpr            
parseComb = (char '(') *> (Comb <$> (zeroOrMore parseSExpr)) <* (char ')')
            
parseSExpr :: Parser SExpr
--parseSExpr = spaces *> parseAtom  <* spaces
parseSExpr = spaces *> (parseAtom <|> parseComb) <* spaces             
