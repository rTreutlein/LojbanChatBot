{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
module OpenCog.Lojban.State where

import OpenCog.AtomSpace hiding (get)
import OpenCog.Lojban.Util
import Control.Monad.Trans.State hiding (State)
import Data.Typeable

import qualified Data.Map as M

type State = ([AtomGen] -> String -> Maybe AtomGen -> Gen LinkT --Gen final link funkction
             , [AtomGen]                        --Argument List
             , String                                --Name
             , Maybe AtomGen                        --Possible context
             , [(AtomGen,AtomGen -> AtomGen)]   --Other Links
             ,[Gen VariableT]                   --List of variables for Questions
             ,String)                                --Input sentence

type IOState a = StateT State IO a

emptyState :: String -> State
emptyState s = (defaultLink,[],"",Nothing,[],[],s)

defaultLink :: [AtomGen] -> String -> Maybe AtomGen -> Gen LinkT
defaultLink args name context =
    let eval = EvaluationLink highTv (PredicateNode name lowTv) (ListLink $ reverse args)
    in case context of
    Nothing -> Gen eval
    Just cxt -> Gen ((\c -> ContextLink highTv c eval) `appGen` cxt :: Atom ContextT)

addAtom :: (AtomGen,AtomGen -> AtomGen) -> IOState ()
addAtom atom = do
    (l,ar,n,c,at,v,s) <- get
    put (l,ar,n,c,atom:at,v,s)

mergeAtoms :: [(AtomGen,AtomGen -> AtomGen)] -> IOState ()
mergeAtoms atoms = do
    (l,ar,n,c,at,v,s) <- get
    put (l,ar,n,c,atoms++at,v,s)

addVar :: (Typeable a,a <~ VariableT) => Atom a -> IOState ()
addVar atom = do
    (l,ar,n,c,at,v,s) <- get
    put (l,ar,n,c,at,Gen atom : v,s)

getVar :: IOState (Gen VariableT)
getVar = do
    (l,ar,n,c,at,v:vs,s) <- get
    put (l,ar,n,c,at,vs,s)
    return v

addArg :: (Typeable a,a <~ AtomT) => Atom a -> IOState ()
addArg atom = do
    (l,ar,n,c,at,v,s) <- get
    put (l,Gen atom : ar,n,c,at,v,s)

getArg :: IOState AtomGen
getArg = do
    (l,e:ar,n,c,at,v,s) <- get
    put (l,ar,n,c,at,v,s)
    return e

addContext :: AtomGen -> String -> IOState ()
addContext atom name = do
    (l,ar,n,c,at,v,s) <- get
    newC <- case c of
        Nothing -> return atom
        Just context -> combineConcepts name atom context
    put (l,ar,n,Just newC,at,v,s)

combineConcepts :: String -> AtomGen -> AtomGen -> IOState AtomGen
combineConcepts name c1 c2 = addAtom i1 >> addAtom i2 >> return c3
    where c3 = Gen $ ConceptNode name lowTv
          i1 = (c1,(\c1 -> Gen $ myInheritanceLink c1 c3))
          i2 = (c2,(\c2 -> Gen $ myInheritanceLink c2 c3))

getContext :: IOState (Maybe AtomGen)
getContext = do
    (l,ar,n,c,at,v,s) <- get
    return c

setLink :: ([AtomGen] -> String -> Maybe AtomGen -> Gen LinkT) -> IOState ()
setLink l = do
    (_,ar,n,c,at,v,s) <- get
    put (l,ar,n,c,at,v,s)

getLink :: IOState ([AtomGen] -> String -> Maybe AtomGen -> Gen LinkT)
getLink = do
    (l,ar,n,c,at,v,s) <- get
    return l

getName :: IOState String
getName = do
    (l,ar,n,c,at,v,s) <- get
    return n

getSentence :: IOState String
getSentence = do
    (l,ar,n,c,at,v,s) <- get
    return s

setName :: String -> IOState ()
setName n = do
    (l,ar,_,c,at,v,s) <- get
    put (l,ar,n,c,at,v,s)
