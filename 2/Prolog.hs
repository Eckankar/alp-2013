module Prolog where
import Data.List (intercalate)

type Id = String

type Predicate = (Id, [Value])

data Value = Fun Predicate
           | Var Id
           | NegationAsFailure Value
           | Cut
           | Fail
           | Unify Id Id
    deriving (Eq)

instance Show Value where
    show (Fun ("nil", [])) = "[]"
    show (Fun ("cons", [x, Fun ("nil", [])])) = "[" ++ show x ++ "]"
    show (Fun ("cons", [x,xs])) = let (_:s) = show xs in "[" ++ show x ++ "," ++ s
    show (Fun (f, [])) = f
    show (Fun (f, vs)) = f ++ "(" ++ intercalate "," (map show vs) ++ ")"
    show (Var i) = i
    show (NegationAsFailure v) = "\\+ " ++ show v
    show (Cut) = "!"
    show (Fail) = "fail"
    show (Unify i j) = i ++ "=" ++ j

data Clause = Clause Predicate [Value]
    deriving (Show, Eq)

type Program = [Clause]
