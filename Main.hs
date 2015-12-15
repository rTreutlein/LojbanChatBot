{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE Rank2Types         #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Main where

import OpenCog.AtomSpace
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class

main :: IO ()
main = do
    as <- newAtomSpace Nothing
    as <: insert behavior
    mainloop as

mainloop :: AtomSpaceObj -> IO ()
mainloop as = do
    text <- getLine
    let atom = (ListLink |> AnchorNode "Lojban" \> ConceptNode text noTv)
    as <: insert atom
    as <: (evaluate (DefinedPredicateNode "mainloop" noTv) >> debug)
    mainloop as

behavior =
  DefineLink
        (DefinedPredicateNode "mainloop" noTv)
        (SatisfactionLink noVars
            (SequentialAndLink
                -- |> (TrueLink \> (SleepLink $ NumberNode 1))
                |> (TrueLink \> translateLojbanToAtomese)
                |> (TrueLink \> deleteLojbanAnchor)
             -- |> (TrueLink \> translateAtomeseToLojban)
             -- |> (TrueLink \> deleteStatmentAnchor)
                |> (TrueLink \> answerQuestion)
             -- |> (TrueLink \> deleteQuestionAnchor)
                \> (TrueLink [])
             -- \> (DefinedPredicateNode "mainloop" noTv)
             ))

deleteLojbanAnchor =
    PutLink
        (DeleteLink $ VariableNode "$text")
        (GetLink noVars $ ListLink |> AnchorNode "Lojban"
                                   \> VariableNode "$text")

deleteStatmentAnchor =
    PutLink
        (DeleteLink $ ListLink |> AnchorNode "StatmentAnchor"
                               \> VariableNode "$text")
        (GetLink noVars $ ListLink |> AnchorNode "StatmentAnchor"
                                   \> VariableNode "$text")

deleteQuestionAnchor =
    PutLink
        (DeleteLink $ ListLink |> AnchorNode "QuestionAnchor"
                               \> VariableNode "$text")
        (GetLink noVars $ ListLink |> AnchorNode "QuestionAnchor"
                                   \> VariableNode "$text")

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
answerQuestion =
    ExecutionOutputLink
        (GroundedSchemaNode "lib: libopencog-lojbantoatoms-0.1.0.0.so\\atomeseToLojban")
        (ListLink \> (GetLink noVars $ ListLink |> AnchorNode "QuestionAnchor"
                                                \> VariableNode "$text"))
