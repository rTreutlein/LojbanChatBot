{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE GADTs              #-}
module OpenCog.Lojban.Util where

import OpenCog.AtomSpace
import Data.Typeable

import Tersmu

pattern CN name <-ConceptNode name _
pattern PN name <-PredicateNode name _
pattern VN name <-VariableNode name

pattern GCN name <-Gen (ConceptNode name _)
pattern GPN name <-Gen (PredicateNode name _)
pattern GVN name <-Gen (VariableNode name)

cCN name tv = ConceptNode name tv
cPN name tv = PredicateNode name tv
cVN name    = VariableNode name

cGCN name tv = Gen $ ConceptNode name tv
cGPN name tv = Gen $ PredicateNode name tv
cGVN name    = Gen $ VariableNode name

isGVN :: AtomGen -> a -> (String -> a) -> a
isGVN (GVN n) _ f = f n
isGVN _ d _       = d

highTv :: TruthVal
highTv = stv 1 0.9

lowTv :: TruthVal
lowTv = stv 0.000001 0.01

toAtomGen :: Gen LinkT -> Gen AtomT
toAtomGen a = Gen `appGen` a

returnGen :: (b <~ a, Monad m, Typeable a) => Atom b -> m (Gen a)
returnGen a = return $ Gen a

myInheritanceLink :: AtomGen -> AtomGen -> Atom InheritanceT
myInheritanceLink a b = InheritanceLink highTv `appGen` a `appGen` b :: Atom InheritanceT

myImplicationLink :: AtomGen -> AtomGen -> Atom ImplicationT
myImplicationLink a b = ImplicationLink highTv `appGen` a `appGen` b :: Atom ImplicationT

ifandOnlyIfLink a b = AndLink highTv [Gen (myImplicationLink a b)
                                     ,Gen (myImplicationLink b a)]

myEvaluationLink :: Gen PredicateT -> AtomGen -> Atom EvaluationT
myEvaluationLink a b = EvaluationLink highTv `appGen` a `appGen` b

mySatisfyingSetLink :: AtomGen -> Atom SatisfyingSetT
mySatisfyingSetLink x = SatisfyingSetLink `appGen` x

myExistsLink :: Gen VariableT -> Gen LinkT -> Atom ExistsT
myExistsLink v link = ExistsLink highTv `appGen` v `appGen` link

myForAllLink :: Gen VariableT -> Gen LinkT -> Atom ForAllT
myForAllLink v link = ForAllLink highTv `appGen` v `appGen` link --Need better name for VarNode

showJbo :: JboShow t => t -> String
showJbo a = evalBindful $ logjboshow True a

if' :: Bool -> a -> a -> a
if' True a _ = a
if' False _ a = a

infixr 1 ?
(?) :: Bool -> a -> a -> a
(?) = if'
