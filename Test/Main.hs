{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
module Main where

import OpenCog.AtomSpace

main :: IO ()
main = mapM testParsing testData >> pure ()

testParsing :: String -> IO (Bool)
testParsing s = do
    let atom = (ListLink |> AnchorNode "Lojban" \> ConceptNode s noTv)
    as <- newAtomSpace Nothing
    as <: insert atom
    as <: insert behavior
    as <: evaluate behavior
    as <: insert getAnswer
    res <- as <: execute getAnswer
    case res of
        Just (Gen (SetLink [Gen (ConceptNode text _)])) -> return True
        _ -> error ("got: " ++ show res ++ "for input: " ++ s)

testData =
  ["mi jimpe do"
  ,"mi jimpe lo gleki"
  ,"mi jimpe lo gleki be do"
  ,"mi jimpe lo nu gleki"
  ,"mi e do jimpe"
  ,"mi vi jimpe"
  ]

getAnswer = GetLink noVars $ ListLink |> AnchorNode "LojbanAnswer"
                                      \> VariableNode "$var"

behavior =
  (SatisfactionLink noVars
            (SequentialAndLink
                |> (TrueLink \> translateLojbanToAtomese)
                \> (TrueLink \> translateAtomeseToLojban)
             ))

translateLojbanToAtomese =
  ExecutionOutputLink
        (GroundedSchemaNode "lib: libopencog-lojbantoatoms-0.1.0.0.so\\lojbanToAtomese")
        (ListLink \> (GetLink noVars $ ListLink |> AnchorNode "Lojban"
                                                \> VariableNode "$text"))

translateAtomeseToLojban =
  ExecutionOutputLink
        (GroundedSchemaNode "lib: libopencog-lojbantoatoms-0.1.0.0.so\\atomeseToLojban")
        (ListLink \> (GetLink noVars $ ListLink |> AnchorNode "StatmentAnchor"
                                                \> VariableNode "$text"))
