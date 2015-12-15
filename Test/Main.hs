module Main where

import OpenCog.AtomSpace
import OpenCog.Lojban

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite = testGroup "Parser Test Suite"
    [ testCase "Simple Sentences" $ assert $ all id <$> mapM testSentence testData
    ]

testSentence :: (String,AtomGen) -> IO (Bool)
testSentence (string,solution) = do
    runOnNewAtomSpace $ lojbanToAtomese string >> debug
    return True

testData =
    [("mi jimpe do",
      Gen $ EvaluationLink highTv
              (PredicateNode "jimpe" lowTv)
              (ListLink |> ConceptNode "mi" lowTv
                        \> ConceptNode "do" lowTv))
    ,("mi jimpe lo gleki",
      Gen $ SetLink |> EvaluationLink highTv
                          (PredicateNode "jimpe" lowTv)
                          (ListLink |> ConceptNode "mi" lowTv
                                    \> ConceptNode "lo gleki" lowTv)
                    \> EvaluationLink highTv
                          (PredicateNode "gleki" lowTv)
                          (ListLink \> ConceptNode "lo gleki" lowTv))]
    {-,("mi jimpe lo gleki be do",)
    ,("mi jimpe lo nu gleki",)
    ,("mi e do jimpe",)]-}
