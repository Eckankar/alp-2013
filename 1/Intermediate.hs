module Intermediate where

type Id = String
type Arg = String
data Atom = Var Id | IntVal Int
    deriving (Eq, Show)

data UnOp = UMinus
    deriving (Eq, Show)
data BinOp = Plus | Minus | Times
    deriving (Eq, Show)
data RelOp = Less | Greater | Equal | LessEqual | GreaterEqual | NotEqual
    deriving (Eq, Show)

data Exp = UnOp UnOp Atom
         | BinOp Id BinOp Atom
         | Mem Atom
         | AVal Atom
         | FCall Id [Arg]
    deriving (Eq, Show)

type Cond = (Id, RelOp, Atom)

data Instruction = Label Id
                 | Set Id Exp
                 | Store Atom Id
                 | Goto Id
                 | If Cond Id Id
                 | Return Id
    deriving (Eq, Show)

type Function = (Id, [Arg], [Instruction])
type Program = [Function]
