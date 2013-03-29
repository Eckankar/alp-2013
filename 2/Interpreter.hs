{-# LANGUAGE DeriveDataTypeable #-}
module Interpreter where

import Prelude hiding (catch)
import Prolog
import Parser
import System.Environment (getArgs)
import Data.Maybe (catMaybes, fromMaybe)
import System.IO (hSetBuffering, BufferMode(..), stdout, stdin)
import Control.Exception (throwIO, try, Exception)
import Data.Typeable (Typeable)
import Data.List (intercalate)
import Control.Monad (void)
--import Debug.Trace (trace, traceShow)

type Substitution = (Id, Value)  -- (i, v) => Var i is bound to v
type Env = (Int, [Substitution]) -- (id used for generating temporary variables, list of substitutions)

{- The empty environment -}
emptyEnv :: Env
emptyEnv = (0, [])

{- Used for signalling that a cut has occured (pp137) -}
data CutExn = CutExn (IO [Env])
    deriving (Typeable)

instance Exception CutExn

instance Show CutExn where
    show _ = "[CutExn]"

{- MGU - unifies a list of value pairs; produces an environment of
 - substitutions needed to perform said unification.
 -
 - Gives none is unification is unsuccessful. -}
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

{- solve, pp135
 - Produces a list of Envs that solve the given goals
 -
 - Usage: solve program env goals
 -}
solve :: [Clause] -> Env -> [Value] -> IO [Env]
--solve _ env gs | trace ("solve\n" ++ show env ++ "\n" ++ show gs ++ "\n") False = undefined
solve _ env []               = return [env]
solve p env (Unify x y : gs) = case unify env [(Var x, Var y)] of
                                    Nothing   -> return []
                                    Just env' -> solve p env' gs
solve p env (Cut : gs)       = do es <- solve p env gs
                                  throwIO $ CutExn $ return es
solve _ _   (Fail : _)       = return []
solve p env (g:gs)           = do v <- try (match g gs env p p)
                                  case v of
                                    Left (CutExn es) -> es
                                    Right es         -> return es

{- match, pp135
 - Usage: match goal goals env clauses program
 -}
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

{- renames, pp135
 - Renames a list of values within a given environment; produces renamed values + new environment -}
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
rename (x:xs) env = let (xs', env') = rename xs env in (x : xs', env')

{- Converts negation as failure to cuts. (pp138) -}
nafToCut :: Int -> [Clause] -> [Clause]
nafToCut _ [] = []
nafToCut i (Clause (n,ps) r : cs) =
    let (i', nafCs, r') = deNaf i [] r []
    in Clause (n,ps) r' : nafToCut i' (nafCs ++ cs)
        where deNaf j r' [] cs' = (j, cs', r')
              deNaf j r' (NegationAsFailure v : vs) cs' =
                    let genId = "#naf_" ++ show j
                    in deNaf (j+1) (r' ++ [Fun (genId, ps)]) vs (cs' ++ [Clause (genId, ps) [v, Cut, Fail], Clause (genId, ps) []])
              deNaf j r' (v:vs) cs' = deNaf j (r' ++ [v]) vs cs'

{- Asks the user for a query, executes it, and displays the solutions -}
processQuery :: Program -> IO ()
processQuery cs = do putStr "?- "
                     qs <- getLine
                     let query = parseQuery qs
                     case query of
                        Nothing -> putStrLn "Syntax error." >> processQuery cs
                        Just q  -> do sols <- solve cs emptyEnv q
                                      displaySolutions sols

{- Prints the bindings in an environment (or yes, if no bindings exist.) -}
printEnv :: Env -> IO ()
printEnv (_, []) = putStr "yes "
printEnv (_, ss) =
    putStr $ intercalate ", " (map (\(i, v) -> i ++ " = " ++ show v) ss) ++ " "

{- Simplifies an environment.
 - That is, bindings to temporary variables are eliminated, and 
 - non-temporary variables are simplified -}
simplifyEnv :: Env -> Env
simplifyEnv (n, ss) = (n, simplifyEnv' ss)
    where simplifyEnv' [] = []
          simplifyEnv' (('#':_, _):ss') = simplifyEnv' ss'
          simplifyEnv' ((i, v):ss') = let v' = simplifyValue v
                                      in if v /= v'
                                         then simplifyEnv' ((i, v'):ss')
                                         else (i, v') : simplifyEnv' ss'
          simplifyValue (Fun (i, vs)) = Fun (i, map simplifyValue vs)
          simplifyValue (Var (x@('#':_))) = fromMaybe (Var x) (lookup x ss)
          simplifyValue v = v

{- Displays solutions as long as ; is pressed -}
displaySolutions :: [Env] -> IO ()
displaySolutions [] = putStrLn "no"
displaySolutions (s:ss) =
    do printEnv $ simplifyEnv s
       c <- getLine
       case c of
          ';':_ -> putChar '\n' >> displaySolutions ss
          _     -> return ()

{- The main function that runs the program -}
main :: IO ()
main = do args <- getArgs
          programs <- mapM parseFile args
          let clauses = concat $ catMaybes programs
          hSetBuffering stdout NoBuffering
          hSetBuffering stdin NoBuffering
          void $ sequence $ repeat $ processQuery $ nafToCut 0 clauses
