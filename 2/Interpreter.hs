{-# LANGUAGE DeriveDataTypeable #-}
module Interpreter where

import Prelude hiding (catch)
import Prolog 
import Parser
import System.Environment (getArgs)
import Data.Maybe (catMaybes, fromJust)
import System.IO (hSetBuffering, BufferMode(..), stdout, stdin)
import Control.Exception (throw, catch, Exception)
import Data.Typeable (Typeable)
import Data.List (intercalate)
import Control.Monad (void)
import Debug.Trace (trace, traceShow)

type Substitution = (Id, Value)
type Env = (Int, [Substitution])

emptyEnv :: Env
emptyEnv = (0, [])

data CutExn = CutExn (IO [Env])
    deriving (Typeable)

instance Exception CutExn

instance Show CutExn where
    show _ = "[CutExn]"

unify :: Env -> [(Value, Value)] -> Maybe Env
--unify env ((a,b):vs) | trace ("unify " ++ show a ++ " with " ++ show b) False = undefined
unify env [] = Just env
unify (env@(i,bs)) ((a,b):vs) = case (a,b) of
    (Var x, Var y) | x == y -> unify env vs
    (Var x, v) -> case lookup x bs of
                    Just w  -> unify env ((w, v):vs)
                    Nothing -> unify (i, (x, v):bs) vs
    (v, Var x) -> unify env $ (Var x, v):vs
    (Fun (f, xs), Fun (g, ys)) | f == g && length xs == length ys
               ->  unify env (zip xs ys ++ vs)
    _ -> Nothing

solve :: [Clause] -> Env -> [Value] -> IO [Env]
--solve _ env gs | trace ("solve\n" ++ show env ++ "\n" ++ show gs ++ "\n") False = undefined
solve _ env []               = return [env]
solve p env (Unify x y : gs) = case unify env [(Var x, Var y)] of
                                    Nothing   -> return []
                                    Just env' -> solve p env' gs
solve p env (Cut : gs)       = throw $ CutExn $ solve p env gs
solve _ _   (Fail : _)       = return []
solve p env (g:gs)           = match g gs env p p `catch` (\ (CutExn es) -> es )

printEnv :: Env -> IO ()
printEnv (_, []) = putStr "yes "
printEnv (_, ss) =
    putStr $ intercalate ", " (map (\(i, v) -> i ++ " = " ++ show v) ss) ++ " "
    
simplifyEnv :: Env -> Env
simplifyEnv (n, ss) = (n, simplifyEnv' ss)
    where simplifyEnv' [] = []
          simplifyEnv' (('#':_, v):ss') = simplifyEnv' ss'
          simplifyEnv' ((i, v):ss') = let v' = simplifyValue v
                                      in if v /= v'
                                         then simplifyEnv' ((i, v'):ss')
                                         else (i, v') : simplifyEnv' ss'
          simplifyValue (Fun (i, vs)) = Fun (i, map simplifyValue vs)
          simplifyValue (Var (x@('#':_))) = fromJust $ lookup x ss
          simplifyValue v = v                  

match :: Value -> [Value] -> Env -> [Clause] -> Program -> IO [Env]
--match g gs (env) (Clause l r : cs) p | trace ("match " ++ show g ++ " - " ++ show l ++ " - (" ++ show env ++ ")") False = undefined
match _ _ _ [] _      = return []
match g gs (env@(i,ss)) (Clause l r : cs) p =
    do let (l1, env1@(i',_)) = rename [Fun l] (i,[])
           env' = (i', ss)
       m <- case unify env' [(g, head l1)] of
                Just (_,ss'') -> let (r1, (i'',_)) = rename r env1 
                                 in solve p (i'', ss'') (r1 ++ gs)
                Nothing       -> return []
       ms <- match g gs env cs p
       return $ m ++ ms

rename :: [Value] -> Env -> ([Value], Env)
rename [] env = ([], env)
rename (Var x : xs) env =
    let (vs, env1@(i, bs)) = rename xs env
    in case lookup x bs of
        Just v  -> (v : vs, env1)
        Nothing -> let v' = Var $ '#' : show i
                   in (v' : vs, (i+1, (x, v') : bs))
rename (Fun (n, vs) : xs) env =
    let (xs', env')  = rename xs env
        (vs', env'') = rename vs env'
    in (Fun (n, vs') : xs', env'')
rename (_:xs) env = rename xs env

nafToCut :: Int -> [Clause] -> [Clause]
nafToCut _ [] = []
nafToCut i (Clause l r : cs) =
    let (i', nafCs, r') = deNaf i [] r []
    in Clause l r' : nafToCut i' (nafCs ++ cs)
        where deNaf j r' [] cs' = (j, cs', r')
              deNaf j r' (NegationAsFailure v : vs) cs' =
                    let genId = "#naf_" ++ show j
                    in deNaf (j+1) (r' ++ [Fun (genId, [])]) vs (cs' ++ [Clause (genId, []) [v, Cut, Fail], Clause (genId, []) []])
              deNaf j r' (v:vs) cs' = deNaf j (r' ++ [v]) vs cs'

processQuery :: [Clause] -> IO ()
processQuery cs = do putStr "?- "
                     qs <- getLine
                     let query = parseQuery qs
                     case query of
                        Nothing -> putStrLn "Syntax error." >> processQuery cs
                        Just q  -> do sols <- solve cs emptyEnv [q]
                                      displaySolutions sols

displaySolutions :: [Env] -> IO ()
displaySolutions [] = putStrLn "no"
displaySolutions (s:ss) =
    do printEnv $ simplifyEnv s
       c <- getLine
       case c of
          ';':_ -> putChar '\n' >> displaySolutions ss
          _     -> return ()

main :: IO ()
main = do args <- getArgs
          programs <- mapM parseFile args
          let clauses = concat $ catMaybes programs
          hSetBuffering stdout NoBuffering
          hSetBuffering stdin NoBuffering
          void $ sequence $ repeat $ processQuery clauses
