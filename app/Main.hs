{-# LANGUAGE DataKinds, GADTs, TypeOperators, Rank2Types, ImpredicativeTypes #-}
module Main where

import Tersmu
import OpenCog.AtomSpace
import OpenCog.Lojban

import Control.Monad
import Debug.Trace
import Data.Maybe
import qualified Data.List as List

main :: IO ()
main = do
    text <- getLine
    parsed <- lojbanToJboText text
    print parsed
    putStrLn "\n"
    case parsed of
        Just p -> runOnNewAtomSpace (convertJboText p >> debug)
        _      -> return ()
    main

mytrace a = traceShow a a

convertJboText :: JboText -> AtomSpace ()
convertJboText [] = return ()
convertJboText (x:xs) = case x of
    TexticuleProp p -> convertJboProp p >> convertJboText xs
    TexticuleFrag f -> convertJboText xs

highTv :: Maybe TruthVal
highTv = stv 1 0.9

lowTv :: Maybe TruthVal
lowTv = stv 0.000001 0.01

myInheritanceLink :: AtomGen -> AtomGen -> Atom InheritanceT
myInheritanceLink a b = InheritanceLink highTv `appGen` a `appGen` b :: Atom InheritanceT

combineConcepts :: String -> AtomGen -> AtomGen -> AtomSpace AtomGen
combineConcepts name c1 c2 = insert i1 >> insert i2 >> return c3
    where c3 = Gen $ ConceptNode name lowTv
          i1 = myInheritanceLink c1 c3
          i2 = myInheritanceLink c2 c3

--type AtomProp = forall a. (Maybe (Atom ConceptT),Maybe (Atom a))
type AtomProp = (Maybe AtomGen,Maybe AtomGen)

genInsert :: Maybe (Gen a) -> AtomSpace ()
genInsert (Just a)  = appGen insert a
genInsert (Nothing) = return ()

convertJboProp :: JboProp -> AtomSpace ()
convertJboProp prop = convertJboProp' prop >>= genInsert . toInsertable

toInsertable :: AtomProp -> Maybe AtomGen
toInsertable (mc,Nothing)         = Nothing
toInsertable (Just c,Just eval)   = Just $ Gen atom
    where atom = ContextLink highTv `appGen` c `appGen` eval :: Atom ContextT
toInsertable (Nothing,atom)  = atom

ifandOnlyIfLink a b = AndLink highTv [Gen (ImplicationLink highTv a b)
                                     ,Gen (ImplicationLink highTv b a)]

convertJboProp' :: JboProp -> AtomSpace AtomProp
convertJboProp' (Rel (Among s) t) = do
    cs <- convertJboTerm s
    ct <- mapM convertJboTerm t
    return (Nothing,Just $ Gen (SubsetLink highTv `appGen` head ct `appGen` cs :: Atom SubsetT)) --XXX is head acceptable here?
convertJboProp' (Rel Equal t) = do
    ct <- mapM convertJboTerm t
    return (Nothing,Just $ Gen (SimilarityLink highTv `appGen` head ct `appGen` last ct :: Atom SimilarityT)) --XXX is head acceptable here?
convertJboProp' (Rel r t) = do
    cr <- convertJboRel r
    ct <- mapM convertJboTerm t
    return (Nothing,Just $ toEval cr ct)
convertJboProp' (Modal m p) = do
    cm <- convertJboModal m
    cp <- convertJboProp' p
    let name = evalBindful $ logjboshow True (Modal m p)
    case cp of
        (Nothing,Just eval) -> return (Just cm,Just eval)
        (Just c, Just eval) -> do
            newC <- combineConcepts name cm c
            return (Just newC,Just eval)
convertJboProp' (Connected And p1 p2) = do
     cp1 <- convertJboProp' p1
     cp2 <- convertJboProp' p2
     return (Nothing,Just $ Gen $ AndLink highTv [fromJust $ toInsertable cp1
                                                 ,fromJust $ toInsertable cp2])
convertJboProp' (Connected Or p1 p2) = do
     cp1 <- convertJboProp' p1
     cp2 <- convertJboProp' p2
     return (Nothing,Just $ Gen $ OrLink highTv [fromJust $ toInsertable cp1
                                                ,fromJust $ toInsertable cp2])
convertJboProp' (Connected Impl p1 p2) = do
     cp1 <- convertJboProp' p1
     cp2 <- convertJboProp' p2
     let a1 = fromJust $ toInsertable cp1
         a2 = fromJust $ toInsertable cp2
         im = ImplicationLink highTv `appGen` a1 `appGen` a2 :: Atom ImplicationT
     return (Nothing,Just $ Gen im)
convertJboProp' (Connected Equiv p1 p2) = do
     cp1 <- convertJboProp' p1
     cp2 <- convertJboProp' p2
     let a1 = fromJust $ toInsertable cp1
         a2 = fromJust $ toInsertable cp2
         eq = ifandOnlyIfLink `appGen` a1 `appGen` a2 :: Atom AndT
     return (Nothing,Just $ Gen eq)
convertJboProp' (Not x) = do
    cp <- convertJboProp' x
    let atom = fromJust $ toInsertable cp
        not  = NotLink highTv `appGen` atom :: Atom NotT
    return (Nothing,Just $ Gen not)
convertJboProp' Eet = error "ParseProblem"
convertJboProp' (Quantified x1 x2 x3) = do
    cq <- convertQuantifier x1
    cmp <- mapM (\x -> convertJboProp' $ x 0) x2 --XXX:What does the Int represent?
    convertJboProp' $ x3 0
convertJboProp' e = error $ traceShow e "JboProp' Incompelte"
--convertJboProp' (NonLogConnected x1 x2 x3) = _convertJboProp'_body

convertQuantifier :: JboQuantifier -> AtomSpace (Atom ConceptT)
--convertQuantifier (MexQuantifier x) = _convertQuantifier_body
convertQuantifier (LojQuantifier x) = convertLojQuantifer x
--convertQuantifier QuestionQuantifier = _convertQuantifier_body
--convertQuantifier (RelQuantifier x) = _convertQuantifier_body

convertLojQuantifer  :: LojQuantifier -> AtomSpace (Atom ConceptT)
convertLojQuantifer Exists = return $ ConceptNode "Exists" lowTv
convertLojQuantifer Forall = return $ ConceptNode "Forall" lowTv
convertLojQuantifer (Exactly x) = return $ ConceptNode (show x) lowTv

conToPred :: Atom ConceptT -> Atom PredicateT
conToPred (ConceptNode name tv) = PredicateNode name tv

convertJboModal :: JboModalOp -> AtomSpace AtomGen
convertJboModal (JboTagged tag mp) = do
    ct <- convertJboTag tag
    cp <- mapM convertJboTerm mp
    case cp of
        Nothing -> return $ Gen ct
        Just c -> do
            let p = conToPred ct
                e = EvaluationLink highTv p (ListLink [c])
            insert e
            return c
convertJboModal NonVeridical = return $ Gen (ConceptNode "nonveridical" noTv)
--convertJboModal (WithEventAs x) = _convertJboModal_body
--convertJboModal QTruthModal = _convertJboModal_body

convertJboTag :: JboTag -> AtomSpace (Atom ConceptT)
convertJboTag (DecoratedTagUnits u) = do
    cu <- mapM convertDTagUnit u
    return $ head cu

convertDTagUnit :: (Show r, Show t) => DecoratedAbsTagUnit r t -> AtomSpace (Atom 'ConceptT)
convertDTagUnit (DecoratedTagUnit tagNahe tagSE tagNai tagUnit) = convertTagUnit tagUnit

convertTagUnit :: (Show r, Show t) => AbsTagUnit r t -> AtomSpace (Atom 'ConceptT)
convertTagUnit (TenseCmavo s) = return (ConceptNode s lowTv)
--convertTagUnit (CAhA x) = _convertTagUnit_body
--convertTagUnit (FAhA { fahaHasMohi = x_fahaHasMohi, fahaCmavo = x_fahaCmavo }) = _convertTagUnit_body
convertTagUnit (ROI { roiroi = x_roiroi, roiIsSpace = x_roiIsSpace, roiQuantifier = x_roiQuantifier }) = do
    cm <- convertAbsMex x_roiQuantifier
    return $ ConceptNode (x_roiroi++cm) lowTv
--convertTagUnit (TAhE_ZAhO { taheZoheIsSpace = x_taheZoheIsSpace, taheZahoCmavo = x_taheZahoCmavo }) = _convertTagUnit_body
convertTagUnit (BAI x) = return (ConceptNode x lowTv)
--convertTagUnit (FIhO x) = _convertTagUnit_body
--convertTagUnit CUhE = _convertTagUnit_body
--convertTagUnit KI = _convertTagUnit_body
convertTagUnit a = error $ traceShow a "Missing TagUnit implementation"

convertAbsMex :: AbsMex r t -> AtomSpace String
convertAbsMex (MexInt x) = return $ show x
--convertAbsMex (Operation x1 x2) = _convertAbsMex_body
--convertAbsMex (ConnectedMex x1 x2 x3 x4) = _convertAbsMex_body
--convertAbsMex (QualifiedMex x1 x2) = _convertAbsMex_body
--convertAbsMex (MexNumeralString x) = _convertAbsMex_body
--convertAbsMex (MexLerfuString x) = _convertAbsMex_body
--convertAbsMex (MexSelbri x) = _convertAbsMex_body
--convertAbsMex (MexSumti x) = _convertAbsMex_body
--convertAbsMex (MexArray x) = _convertAbsMex_body

toEval :: Atom PredicateT -> [AtomGen] -> AtomGen
toEval p c = Gen $ EvaluationLink highTv p (ListLink c)

showJbo a = evalBindful $ logjboshow True a

convertJboRel :: JboRel -> AtomSpace (Atom PredicateT)
convertJboRel (Brivla s) = return (PredicateNode s lowTv)
convertJboRel (Moi t c) = do
    let name = showJbo t
    return $ PredicateNode (c++name) lowTv
convertJboRel (Tanru jbovpred jborel) = do
    (PredicateNode name _) <- convertJboRel jborel
    cp <- convertJboVPred jbovpred
    return $ PredicateNode (cp++name) lowTv
convertJboRel (AbsProp a prop) = do
    ca <- convertAbstractor a
    (mc,Just atom) <- convertJboProp' prop
    let name = evalBindful $ logjboshow True prop
    case mc of
        Just c  -> insert (ContextLink highTv `appGen` c `appGen` atom :: Atom ContextT)
        Nothing -> insert $ EquivalenceLink highTv
                                (LambdaLink
                                    (VariableNode "1")
                                    (EvaluationLink highTv
                                        (PredicateNode (ca++name) lowTv)
                                        (ListLink
                                            \> VariableNode "1")))
                                (LambdaLink
                                    (VariableNode "2")
                                    (ContextLink highTv
                                        (VariableNode "2")
                                        `appGen` atom :: Atom ContextT))
    return $ PredicateNode (ca++name) lowTv
convertJboRel (AbsPred a pred) = do
    let name = evalBindful $ logjboshow True (AbsPred a pred)
    return (PredicateNode name lowTv)
convertJboRel (ScalarNegatedRel _s rel) = convertJboRel rel --XXX ignoring s
--convertJboRel (Among s) = convertJboTerm s >>= (\c -> return $ conToPred c)
--convertJboRel (TanruConnective s1 s2 s3) = _convertJboRel_body
--convertJboRel (UnboundBribasti s) = _convertJboRel_body
--convertJboRel (BoundRVar s) = _convertJboRel_body
--convertJboRel (RVar s) = _convertJboRel_body
--convertJboRel (OperatorRel s) = _convertJboRel_body
--convertJboRel (TagRel s) = _convertJboRel_body
convertJboRel  a = error $ traceShow a "JboRel Incompelte"

convertJboVPred :: JboVPred -> AtomSpace String
convertJboVPred a = return $ evalBindful $ logjboshow True a
    --let pred = vPredToPred vpred
    --    prop = pred (BoundVar 1)
    --(_,Just (EvaluationLink _ p _)) <- convertJboProp' prop
    --return $ getName p

--convertJboNpred :: JboNPred -> AtomSpace String
--convertJboNpred (JboNPred arity pred) =return $ evalBindful $ logjboshow True 
    --let args = map BoundVar $ take arity [1..]
    --    prop = pred args
    --(_,Just (EvaluationLink _ p _)) <- convertJboProp' prop
    --return $ getName p

convertAbstractor :: Abstractor -> AtomSpace String
convertAbstractor (NU s) = return s

--getName :: (a <~ NodeT) => Atom a -> String
--getName a = List.delete '"' $ List.delete '"' $ (words . show $ a)!!1
--
returnGen a = return $ Gen a

convertJboTerm :: JboTerm -> AtomSpace AtomGen
convertJboTerm (NonAnaph s)                  = returnGen $ ConceptNode s lowTv
convertJboTerm (Constant n _)                = returnGen $ ConceptNode (show n) lowTv
convertJboTerm (Value v)                     = convertJboMex v
convertJboTerm (Named s)                     = returnGen $ ConceptNode s lowTv
convertJboTerm (BoundVar n)                  = returnGen $ ConceptNode "" lowTv
convertJboTerm (UnboundSumbasti x)           = convertSumtiAtom x
convertJboTerm (JboQuote (ParsedQuote text)) = returnGen $ ConceptNode (show text) lowTv
convertJboTerm Unfilled                      = returnGen $ ConceptNode "zo'e" $ stv 1 1
convertJboTerm (JoikedTerms "ce" t1 t2)      = do
    ct1 <- convertJboTerm t1
    ct2 <- convertJboTerm t2
    returnGen (SetLink [ct1,ct2])
--convertJboTerm (Var x) = _convertJboTerm_body
--convertJboTerm (Named x) = _convertJboTerm_body
--convertJboTerm (PredNamed x) = _convertJboTerm_body
--convertJboTerm (JboErrorQuote x) = _convertJboTerm_body
--convertJboTerm (JboNonJboQuote x) = _convertJboTerm_body
--convertJboTerm (TheMex x) = _convertJboTerm_body
--convertJboTerm (Valsi x) = _convertJboTerm_body
--convertJboTerm (QualifiedTerm x1 x2) = _convertJboTerm_body
convertJboTerm a                             = error $ traceShow a "JboTerm Incompelte"

convertSumtiAtom :: SumtiAtom -> AtomSpace AtomGen
convertSumtiAtom (LerfuString s) = convertLerfu $ head s

convertLerfu :: Lerfu -> AtomSpace AtomGen
convertLerfu (LerfuChar s) = returnGen $ ConceptNode [s] lowTv
convertLerfu (LerfuValsi s) = returnGen $ ConceptNode s lowTv

convertJboMex :: JboMex -> AtomSpace AtomGen
convertJboMex (MexInt i) = returnGen $ ConceptNode (show i) lowTv
