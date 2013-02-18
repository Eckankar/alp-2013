import Parser
import Intermediate
import Test.HUnit (Test(..), Counts(), assertEqual, runTestTT)
import System.FilePath (replaceExtension, replaceBaseName)

-- ASTs {{{
fibAST :: Program
fibAST = [
    ("fib", ["n"], [
        Set "a" (AVal $ IntVal 0),
        Set "b" (AVal $ IntVal 1),
        Set "z" (AVal $ IntVal 0),
        Label "loop",
        If ("n", Equal, Var "z") "end" "body",
        Label "body",
        Set "t" (BinOp "a" Plus (Var "b")),
        Set "a" (AVal $ Var "b"),
        Set "b" (AVal $ Var "t"),
        Set "n" (BinOp "n" Minus (IntVal 1)),
        Set "z" (AVal $ IntVal 0),
        Goto "loop",
        Label "end",
        Return "a"
    ])
  ]
-- }}}

fileTestCases :: [(String, Program)]
fileTestCases = [ ("fib", fibAST) ]

testWithFile :: (String, Program) -> IO Test
testWithFile (f, ast) =
    do got <- parseFile filePath
       return $ TestCase $ assertEqual ("parseFile (" ++ f ++ ")")
                                         (Just ast) got
    where file = replaceExtension f ".int"
          filePath = replaceBaseName "programs/" file

fileTests :: IO Test
fileTests = fmap TestList $ mapM testWithFile fileTestCases

main :: IO Counts
main = do t <- fileTests
          runTestTT t
