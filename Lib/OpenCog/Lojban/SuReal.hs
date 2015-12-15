{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
module OpenCog.Lojban.SuReal where

import OpenCog.AtomSpace hiding (get,insert)
import OpenCog.Lojban.Util
import Data.Foldable
import Data.List
import Control.Monad.Trans.State hiding (State)
import Control.Monad.IO.Class

import qualified Data.Map as M

type SuRealState = (M.Map String String) --Map from named to name

type SuRealStateT a = StateT SuRealState AtomSpace a

atomeseToLojban :: AtomGen -> AtomSpace AtomGen
atomeseToLojban a@(Gen (ListLink [(Gen (SetLink [(Gen (SetLink (s:r)))]))])) = do
    liftIO $ print a
    liftIO $ putStrLn "--------------------------"
    liftIO $ print r
    liftIO $ putStrLn "--------------------------"
    (v,state) <- runStateT (mapM getCmene r) M.empty
    lojban <- evalStateT (sayLink s) state
    return $ Gen $ ListLink |> AnchorNode "LojbanAnswer"
                       \> ConceptNode lojban noTv
atomeseToLojban a = do
    liftIO $ putStrLn "empty input:"
    liftIO $ print a
    return (Gen $ SetLink [])

getCmene :: AtomGen -> SuRealStateT ()
getCmene (Gen (EvaluationLink _ (PredicateNode "cmene" _)(ListLink [a,b]))) = do
    (Gen (ConceptNode name  _)) <- pure a
    (Gen (ConceptNode named _)) <- pure b
    (nmap) <- get
    put (M.insert named name nmap)
getCmene a = pure ()

(+|+) :: String -> String -> String
(+|+) = (\a b -> a ++ (' ':b))

sayLink :: AtomGen -> SuRealStateT String
sayLink a = do
    r <- sayLink' a
    return (prun r)

sayLink' :: AtomGen -> SuRealStateT String
sayLink' (Gen (EvaluationLink _ pred (ListLink args))) = do
    (h:r) <- mapM sayArgument args
    lp    <- sayPredicate (Gen pred)
    return $ h +|+ lp +|+ (foldl (+|+) "" r)
sayLink' a = error $ "sayLink not implemented for: " ++ show a

saySubLink :: AtomGen -> SuRealStateT String
saySubLink a = do
    r <- saySubLink' a
    return (prun r)

prun :: String -> String
prun a = case " zo'e" `isSuffixOf` a of
    True -> prun $ take (length a - 5) a
    False -> a

saySubLink' :: AtomGen -> SuRealStateT String
saySubLink' (Gen (EvaluationLink _ pred (ListLink args))) = do
    sa <- mapM sayArgument args
    sp <- sayPredicate (Gen pred)
    let mod = case elemIndex "#var#" sa of
              {Just 0 -> ""; Just 1 -> "se "; Just 2 -> "te ";
              Just 3 -> "ve "; Just 4 -> "xe ";
              Nothing -> error "no var in SatisfyingSetLink"}
        rargs = tail $ swap (head sa) ("#var#") sa
    return $ "lo" +|+ mod ++ sp +|+ (foldl (+|+) "" rargs)
saySubLink' (Gen (ContextLink _ c l)) = do
    let (ConceptNode cname _) = c
    link <- saySubLink (Gen l)
    return $ case "nonveridical" `isInfixOf` cname of
              True -> "le " ++ (drop 3 link)
              False -> link
saySubLink' a = error $ "saySubLink not implemented for: " ++ show a

swap a b = map (\x -> if x == a then b else if x == b then a else x)

sayArgument :: AtomGen -> SuRealStateT String
sayArgument (Gen (ConceptNode s _)) = do
    nmap <- get
    return $ case elemIndex '#' s of
        Just i -> take i s
        Nothing -> case M.lookup s nmap of
            Just r -> "la" +|+ r
            Nothing -> case s of
                "zo'e" -> " zo'e"
                _      -> error $ "Concept not named or a instance: " ++ s

sayArgument (Gen (VariableNode _)) = return "#var#"
sayArgument (Gen (SatisfyingSetLink l)) = saySubLink (Gen l)
sayArgument a = error $ "sayArgument not implemented for: " ++ show a

sayPredicate :: AtomGen -> SuRealStateT String
sayPredicate (Gen (PredicateNode s _)) = return s
sayPredicate a = error $ "sayPredicate not implemented for: " ++ show a
