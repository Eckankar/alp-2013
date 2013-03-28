{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TupleSections #-}

module Parser
  ( parseFile
  , parseString
  , parseQuery
  ) where

import Prolog 
import Control.Monad (ap, MonadPlus(mzero, mplus), liftM)
import Control.Applicative ((<$>), Applicative(..),
                            Alternative(empty, (<|>)))
import Text.ParserCombinators.ReadP
import Data.Char (isAlphaNum, isUpper, isLower, isDigit)

-- Instances for Applicative ReadP and Alternative ReadP aren't in base-4.5
{-instance Applicative ReadP where
    pure = return
    (<*>) = ap

instance Alternative ReadP where
    empty = mzero
    (<|>) = mplus 
-}    
    
-- Char preceded by whitespace
schar :: Char -> ReadP Char
schar c = skipSpaces >> char c

-- String preceded by whitespace
sstring :: String -> ReadP String
sstring s = skipSpaces >> string s

parenthesized :: ReadP a -> ReadP a
parenthesized = between (schar '(') (schar ')')

query :: ReadP Value
query = do v <- value
           skipSpaces
           eof
           return v

program :: ReadP Program
program = do cs <- many1 clause
             skipSpaces
             eof
             return cs

clause :: ReadP Clause
clause = (do fp <- predicate
             sstring ":-"
             vs <- sepBy1 value (schar ',')
             schar '.'
             return $ Clause fp vs)
    <|> (do fp <- predicate
            schar '.'
            return $ Clause fp [])

predicate :: ReadP Predicate
predicate = do i <- predId
               as <- option [] $ parenthesized $ sepBy1 value (schar ',')
               return (i, as)

predId :: ReadP String
predId = do skipSpaces
            i  <- satisfy ((||) <$> isLower <*> isDigit)
            is <- munch isAlphaNum
            return $ i:is

varId :: ReadP String
varId = do skipSpaces
           i  <- satisfy isUpper
           is <- munch isAlphaNum
           return $ i:is

value :: ReadP Value
value = fmap Fun predicate
    <|> fmap Var varId
    <|> fmap Fun list
    <|> (sstring "\\+" >> fmap NegationAsFailure value)
    <|> (do v1 <- varId
            schar '='
            v2 <- varId
            return $ Unify v1 v2)

list :: ReadP Predicate
list = do schar '['
          ls <- listInner
          schar ']'
          return ls

listInner :: ReadP Predicate
listInner = return ("nil", [])
        <|> (do h <- value
                schar '|'
                t <- value
                return ("cons", [h, t]))
        <|> (do h <- value
                schar ','
                t <- listInner
                return ("cons", [h, Fun t]))
        <|> do h <- value
               return ("cons", [h, Fun ("nil", [])])

parseQuery :: String -> Maybe Value
parseQuery s = case readP_to_S query s of
                    [(v,"")] -> Just v
                    _        -> Nothing

parseString :: String -> Maybe Program
parseString s = case readP_to_S program s of
                    [(p,"")] -> Just p
                    _        -> Nothing

parseFile :: FilePath -> IO (Maybe Program)
parseFile = liftM parseString . readFile
