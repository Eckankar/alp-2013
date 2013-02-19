module Intermediate where

import Data.List (intercalate)

type Id = String
type Arg = String
data Atom = Var Id | IntVal Int
    deriving Eq

data UnOp = UMinus
    deriving Eq
data BinOp = Plus | Minus | Times
    deriving Eq
data RelOp = Less | Greater | Equal | LessEqual | GreaterEqual | NotEqual
    deriving Eq

data Exp = UnOp UnOp Atom
         | BinOp Id BinOp Atom
         | Mem Atom
         | AVal Atom
         | FCall Id [Arg]
    deriving Eq

type Cond = (Id, RelOp, Atom)

data Instruction = Label Id
                 | Set Id Exp
                 | Store Atom Id
                 | Goto Id
                 | If Cond Id Id
                 | Return Id
    deriving Eq

type Function = (Id, [Arg], [Instruction])
type Program = [Function]

instance Show UnOp where
    show UMinus = "-"

instance Show BinOp where
    show Plus = "+"
    show Minus = "-"
    show Times = "*"

instance Show RelOp where
    show Less = "<"
    show Greater = ">"
    show Equal = "="
    show LessEqual = "<="
    show GreaterEqual = ">="
    show NotEqual = "!="

instance Show Exp where
    show (UnOp uo a) = show uo ++ show a
    show (BinOp i bo a) = i ++ " " ++ show bo ++ " " ++ show a
    show (Mem a) = "M[" ++ show a ++ "]"
    show (AVal a) = show a
    show (FCall i as) = i ++ "(" ++ intercalate ", " (map show as) ++ ")"

instance Show Instruction where
    show (Label i) = "LABEL " ++ i
    show (Set i e) = i ++ " := " ++ show e
    show (Store a i) = "M[" ++ show a ++ "] := " ++ i
    show (Goto i) = "GOTO " ++ i
    show (If (i, ro, a) t f) = "IF " ++ i ++ " " ++ show ro ++ " " ++ show a ++
                               " THEN " ++ t ++ " ELSE " ++ f
    show (Return i) = "RETURN " ++ i

instance Show Atom where
    show (Var i) = i
    show (IntVal i) = show i
