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
    as <: insert deleteAnswerAnchor
    mainloop as

mainloop :: AtomSpaceObj -> IO ()
mainloop as = do
    text <- getLine
    let atom = Link "ListLink" [Node "AnchorNode" "Lojban" noTv
                               ,Node "ConceptNode" text noTv
                               ] noTv
    as <: insert atom
    as <: evaluate $ Node "DefinedPredicateNode" "mainloop" noTv
  --as <: debug
    as <: printAnswer
    mainloop as

printAnswer :: AtomSpace ()
printAnswer = do
    insert getAnswer
    response <- execute getAnswer
    execute $ deleteAnswerAnchor
    case response of
        Just (Link "SetLink" [_,Node "ConceptNode" text _] _) -> liftIO $ print text
        _ -> return ()

getAnswer = Link "GetLink" [Link "ListLink" [Node "AnchorNode" "LojbanAnswer" noTv
                                            ,Node "VariableNode" "$var" noTv
                                            ] noTv
                           ] noTv

deleteAnswerAnchor = deleteAnchor "LojbanAnswer" True

behavior =
  Link "DefineLink"
        [Node "DefinedPredicateNode" "mainloop" noTv
        ,Link "SatisfactionLink"
            [Link "SequentialAndLink"
             -- , Link "TrueLink" [deleteAnswerAnchor] noTv
                [ Link "TrueLink" [translateLojbanToAtomese] noTv
                , Link "TrueLink" [deleteAnchor "Lojban" False] noTv
             -- , Link "TrueLink" [translateAtomeseToLojban] noTv
             -- , Link "TrueLink" [deleteStatmentAnchor] noTv
                , Link "TrueLink" [answerQuestion] noTv
                , Link "TrueLink" [deleteAnchor "QuestionAnchor" True] noTv
                , Link "TrueLink" [] noTv
                ] noTv
             -- , Node "DefinedPredicateNode" "mainloop" noTv
            ] noTv
        ] noTv

deleteAnchor :: String --Specify what AnchorNode to look for
             -> Bool --True only Link will be delted
                  --False the linked content will be delted
             -> Atom
deleteAnchor a b = if b then deletelink else deletelinked
    where deletelink = Link "PutLink"
                        [Link "DeleteLink" [Link "ListLink" [ Node "AnchorNode" a noTv
                                                            , Node "VariableNode" "$text" noTv
                                                            ] noTv
                                           ] noTv
                        ,Link "GetLink" [Link "ListLink" [Node "AnchorNode" a noTv
                                                         ,Node "VariableNode""$text" noTv
                                                         ] noTv
                                        ] noTv
                        ] noTv
          deletelinked = Link "PutLink"
                        [Link "DeleteLink" [Node "VariableNode" "$text" noTv] noTv
                        ,Link "GetLink" [Link "ListLink" [Node "AnchorNode" a noTv
                                                         ,Node "VariableNode" "$text" noTv
                                                         ] noTv
                                        ] noTv
                        ] noTv



translateLojbanToAtomese =
    Link "ExecutionOutputLink"
        [Node "GroundedSchemaNode"
            "lib: libopencog-lojbantoatoms-0.1.0.0.so\\lojbanToAtomese" noTv
        ,Link "ListLink"
            [Link "GetLink"
                [Link "ListLink"
                    [Node "AnchorNode" "Lojban" noTv
                    ,Node "VariableNode" "$text" noTv
                    ] noTv
                ] noTv
             ] noTv
        ]noTv

translateAtomeseToLojban =
    Link "ExecutionOutputLink"
        [Node "GroundedSchemaNode"
            "lib: libopencog-lojbantoatoms-0.1.0.0.so\\atomeseToLojban" noTv
        ,Link "ListLink"
            [Link "GetLink"
                [Link "ListLink"
                    [Node "AnchorNode" "StatmentAnchor" noTv
                    ,Node "VariableNode" "$text" noTv
                    ] noTv
                ] noTv
            ] noTv
        ] noTv

answerQuestion =
    Link "ExecutionOutputLink"
        [Node "GroundedSchemaNode"
            "lib: libopencog-lojbantoatoms-0.1.0.0.so\\atomeseToLojban" noTv
        ,Link "ListLink"
            [Link "GetLink"
                [Link "ListLink"
                    [Node "AnchorNode" "QuestionAnchor" noTv
                    ,Node "VariableNode" "$text" noTv
                    ] noTv
                ] noTv
            ] noTv
        ] noTv
