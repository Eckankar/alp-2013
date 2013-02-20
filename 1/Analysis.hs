module Analysis
    ( liveness
    , Liveness()
    , livenessFile
    ) where

import Intermediate
import Parser
import qualified Data.Map as M
import qualified Data.Array as A
import qualified Data.Set as S

data Liveness = Liveness [(Id, [(Instruction, S.Set Id, S.Set Id)])]

pad :: a -> Int -> [a] -> [a]
pad e n ls = ls ++ replicate (n - length ls) e

instance Show Liveness where
    show (Liveness fs) = unlines $ map formatFun fs
        where formatFun (name, is) =
                "== " ++ name ++ " ==\n" ++
                line ++ "\n" ++
                "| " ++ pad ' ' 40 "Instruction" ++ " | " ++ pad ' ' 20 "In" ++ " | " ++ pad ' ' 20 "Out" ++ " |\n" ++
                line ++ "\n" ++
                foldl (\a (i, ins, out) ->
                    a ++ "| " ++
                    pad ' ' 40 (show i) ++ " | " ++
                    pad ' ' 20 (show $ S.toList ins) ++ " | " ++
                    pad ' ' 20 (show $ S.toList out) ++ " | " ++ "\n"
                ) "" is ++ line
              line = pad '-' 90 ""

livenessFile :: String -> IO (Maybe Liveness)
livenessFile filename = do mp <- parseFile filename
                           return $ fmap liveness mp

liveness :: Program -> Liveness
liveness = Liveness . map livenessFun

livenessFun :: Function -> (Id, [(Instruction, S.Set Id, S.Set Id)])
livenessFun (name, _, is) =
    let n    = length is
        ls   = labels is
        suc  = arrayFromList $ map (succSet ls) (zip [0..] is)
        gen  = arrayFromList $ map (S.fromList . genSet) is
        kill = arrayFromList $ map (S.fromList . killSet) is
        initial = [S.empty | _ <- [1..n]]
        (ins, out) = livenessIter n suc gen kill initial initial
    in (name, zip3 is ins out)

livenessIter :: Int ->
                A.Array Int [Int] ->
                A.Array Int (S.Set Id) ->
                A.Array Int (S.Set Id) ->
                [S.Set Id] -> [S.Set Id] ->
                ([S.Set Id], [S.Set Id])
livenessIter n suc gen kill pins pout =
    let pinsa = arrayFromList pins
        pouta = arrayFromList pout
        (ins, out) = foldr (\i (is, os)->
                (S.union (gen A.! i) (S.difference (pouta A.! i) $ kill A.! i) : is,
                 S.unions (map (pinsa A.!) $ suc A.! i) : os)
            ) ([], []) [0 .. n-1]
    in if   ins == pins && out == pout
       then (ins, out)
       else livenessIter n suc gen kill ins out

arrayFromList :: [a] -> A.Array Int a
arrayFromList ls = A.listArray (0, length ls - 1) ls

genSet :: Instruction -> [Id]
genSet (Set _ (AVal (Var y))) = [y]
genSet (Set _ (UnOp _ (Var y))) = [y]
genSet (Set _ (BinOp y _ (Var z))) = [y,z]
genSet (Set _ (BinOp y _ _)) = [y]
genSet (Set _ (Mem (Var y))) = [y]
genSet (Store (Var x) y) = [x, y]
genSet (Store _ y) = [y]
genSet (If (x, _, Var y) _ _) = [x, y]
genSet (If (x, _, _) _ _) = [x]
genSet (Set _ (FCall _ as)) = as
genSet (Return x) = [x]
genSet _ = []

killSet :: Instruction -> [Id]
killSet (Set x _) = [x]
killSet _ = []

succSet :: M.Map String Int -> (Int, Instruction) -> [Int]
succSet ls (_, Goto i) = [ls M.! i]
succSet ls (_, If _ x y) = [ls M.! x, ls M.! y]
succSet _ (_, Return _) = []
succSet _ (i, _) = [i+1]

labels :: [Instruction] -> M.Map Id Int
labels = snd . foldl (\(i, m) e ->
    (i+1,
     case e of
            Label l -> M.insert l i m
            _       -> m)
    ) (0, M.empty)
