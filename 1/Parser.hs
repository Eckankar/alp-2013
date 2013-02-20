{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TupleSections #-}

module Parser
  ( parseFile
  , parseString
  ) where

import Intermediate 
import Control.Monad (ap, MonadPlus(mzero, mplus), liftM)
import Control.Applicative (Applicative(..),
                            Alternative(empty, (<|>)))
import Text.ParserCombinators.ReadP
import Data.Char (isAlpha, isDigit)

-- Instances for Applicative ReadP and Alternative ReadP aren't in base-4.5
instance Applicative ReadP where
    pure = return
    (<*>) = ap

instance Alternative ReadP where
    empty = mzero
    (<|>) = mplus

-- Char preceded by whitespace
schar :: Char -> ReadP Char
schar c = skipSpaces >> char c

-- String preceded by whitespace
sstring :: String -> ReadP String
sstring s = skipSpaces >> string s

bracketed :: ReadP a -> ReadP a
bracketed = between (schar '[') (schar ']')

parenthesized :: ReadP a -> ReadP a
parenthesized = between (schar '(') (schar ')')

program :: ReadP Program
program = do fs <- many1 function
             skipSpaces
             eof
             return fs

function :: ReadP Function
function = do i <- ident
              as <- args
              body <- bracketed $ many1 instruction
              return (i, as, body)

ident :: ReadP Id
ident = skipSpaces >> munch1 isAlpha

num :: ReadP Int
num = do skipSpaces
         s  <- option "" $ sstring "-"
         ds <- munch1 isDigit
         return $ read (s ++ ds)

args :: ReadP [Arg]
args = parenthesized $ sepBy1 ident (schar ',')

instruction :: ReadP Instruction
instruction =
      liftM Label (sstring "LABEL" >> ident)
  <|> (do i <- ident
          sstring ":="
          e <- expr
          return $ Set i e)
  <|> (do addr <- mem
          sstring ":="
          i <- ident
          return $ Store addr i)
  <|> liftM Goto (sstring "GOTO" >> ident)
  <|> (do sstring "IF"
          c <- cond
          sstring "THEN"
          th <- ident
          sstring "ELSE"
          el <- ident
          return $ If c th el)
  <|> liftM Return (sstring "RETURN" >> ident)

expr :: ReadP Exp
expr = (do uo <- unop
           a  <- atom
           return $ UnOp uo a)
   <|> (do i  <- ident
           bo <- binop
           a  <- atom
           return $ BinOp i bo a)
   <|> (do sstring "CALL"
           i  <- ident
           as <- args
           return $ FCall i as)
   <|> liftM Mem mem
   <|> liftM AVal atom

mem :: ReadP Atom
mem = schar 'M' >> bracketed atom

atom :: ReadP Atom
atom = liftM Var ident
   <|> liftM IntVal num

cond :: ReadP Cond
cond = do i <- ident
          ro <- relop
          a <- atom
          return (i, ro, a)

unop :: ReadP UnOp
unop = schar '-' >> return UMinus

binop :: ReadP BinOp
binop = (schar '-' >> return Minus)
    <|> (schar '+' >> return Plus)
    <|> (schar '*' >> return Times)
    <|> (schar '/' >> return Division)
    <|> (schar '%' >> return Modulo)

relop :: ReadP RelOp
relop = (sstring ">=" >> return GreaterEqual)
    <|> (sstring "<=" >> return LessEqual)
    <|> (sstring "!=" >> return NotEqual)
    <|> (schar '<' >> return Less)
    <|> (schar '>' >> return Greater)
    <|> (schar '=' >> return Equal)

parseString :: String -> Maybe Program
parseString s = case readP_to_S program s of
                    [(p,"")] -> Just p
                    _        -> Nothing

parseFile :: FilePath -> IO (Maybe Program)
parseFile = liftM parseString . readFile
