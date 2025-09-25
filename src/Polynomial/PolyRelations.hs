module Polynomial.PolyRelations where 
import Diagnosis.RelationTree hiding (embed)
import Polynomial.ExprPoly 
import SymbolRoot.ComplexRoot hiding (simplify)
import qualified SymbolRoot.ComplexRoot as CR (simplify)
import Data.List (sortBy)
import Polynomial.PolyStrat

type OrListC = OrList String ComRoot
type PolyEqC = PolyEq String ComRoot
type ExprPolyC = ExprPoly String ComRoot 

(.=>) :: Bool -> Bool -> Bool
p .=> q = not p || p && q 

normal :: PolyEq String ComRoot -> PolyEq String ComRoot 
normal p = 
   (fmap CR.simplify $ monicFactors $ mapTerms (a *) $ toZero p) :=: 0
      where a = case leadCoeff (toZero p) of 
                0  -> 1
                a' -> N (CR.simplify $ 1/a')

normalOr :: OrList String ComRoot -> OrList String ComRoot
normalOr = setNubOr . mapToOr normal

distributed :: OrList String ComRoot -> OrList String ComRoot -> Bool
distributed p q = 
   ((normalOr q) `elem`) . map normalOr . distSet $ p

matchEqs :: 
   OrList String ComRoot -> 
      OrList String ComRoot -> [(PolyEqC, PolyEqC)]
matchEqs p q = zip (sortBy c $ or2list p) (sortBy c $ or2list q)
   where c x y = compare (normal x) (normal y) 

noZeroDegree :: OrList String ComRoot -> Test (OrList String ComRoot)
noZeroDegree b = binary f "no equation of zero degree"
   where f = all ((/=0) . degree . toZero) $ or2list b

expectedNrEqs :: 
   OrList String ComRoot -> Test (OrList String ComRoot)
expectedNrEqs b = test f "expected number of equations"
   where f a = (nrOfEqs $ nubOr b) == (nrOfEqs $ nubOr a)

biggerNrEqs :: 
   OrList String ComRoot -> Test (OrList String ComRoot)
biggerNrEqs b = test f "bigger number of equations"
   where f a = (nrOfEqs $ nubOr b) >= (nrOfEqs $ nubOr a)

expectedNormal :: OrList String ComRoot -> Test (OrList String ComRoot)
expectedNormal b = test f "expected normal form"
   where f a = normalOr a == normalOr b

expectedNrOfTerms :: 
   OrList String ComRoot -> Test (OrList String ComRoot)
expectedNrOfTerms b = test f "expected number of terms"
   where 
      f a = all c (matchEqs a b)
      c (a', b') = nrt a' >= nrt b'
      nrt = nrOfTerms . reduceIDs . toZero 

obtainedByDist :: 
   OrList String ComRoot -> 
      OrList String ComRoot-> Test (OrList String ComRoot)
obtainedByDist a b = binary (distributed a b) "obtained by distribution" 

expectedZero ::
   OrList String ComRoot -> Test (OrList String ComRoot)
expectedZero b = test f "derived to zero as expected"
   where 
      f a = degLT2 b || all c (matchEqs a b) 
      c (a', b') =  (z a' .=> z b' && 
                   lq a' .=> (z b'.=> z a'))
      z (l:=:r) = l == 0 || r == 0
      degLT2 = all ((<2) . degree . toZero) . or2list
      lq x = case normal x of 
             (N _ :*: ((_ :+: _):^:2)) :+: N _ :=: 0 -> True 
             N _ :*: ((_ :+: _):^:2) :=: 0 -> True
             ((_ :+: _):^:2) :+: N _  :=: 0 -> True
             (_ :+: _):^:2 :=: 0 -> True 
             _ -> False

allDerivedToZero :: 
   OrList String ComRoot -> Test (OrList String ComRoot)
allDerivedToZero b = binary f "equation(s) derived to zero" 
   where f = all z (or2list b)
         z (l:=:r) = l == 0 || r == 0

expectedSquares :: 
   OrList String ComRoot -> Test (OrList String ComRoot)
expectedSquares b = test f "has squares as expected"
   where 
      f a = all c (matchEqs a b)
      c (a', b') = s a' .=> s b'
      s x = not $ null [ p 
                       | x' <- terms2list (toZero x)
                       , (p:+:_):^:2 <- factors2list x']

expectedMonic :: 
   OrList String ComRoot -> Test (OrList String ComRoot)
expectedMonic b = test f "is monic as expected"
   where 
      f a = all c (matchEqs a b)
      c (a', b') = m a' == m b'
      m x = leadCoeff (toZero x) == 1 

expectedLead :: 
   OrList String ComRoot -> Test (OrList String ComRoot)
expectedLead b = test f "expected lead coefficient"
   where 
      f a = all c (matchEqs a b) 
      c (a', b') = m a' == m b' 
                   || m b' == 1 
                   || degree (toZero b') < 2 
      m x = abs $ leadCoeff (toZero x) 


expectedDisc :: 
   OrList String ComRoot -> 
      Test (OrList String ComRoot)
expectedDisc b = test f "discriminant is as expected"
   where 
      f a = all c (matchEqs a b) 
      c (a', b') = isDisc a' == isDisc b' 
      isDisc (Var "D" :=: _) = True
      isDisc _ = False

polyTree :: 
   OrList String ComRoot -> 
      OrList String ComRoot -> 
         RelationTree (OrList String ComRoot)
polyTree a b = 
         A (noZeroDegree b)
         .>> A (expectedDisc b)
         .>> (A (expectedNrEqs b) |> A (biggerNrEqs b) .>> stop)
         .>> (A (expectedNormal b) |> A (obtainedByDist a b) .>> stop) 
         .>> A (expectedNrOfTerms b)
         .>> (A (expectedZero b) |> A (allDerivedToZero b) .>> stop)
         .>> A (expectedSquares b)
         .>> (A (expectedLead b) |> A (obtainedByDist a b) .>> stop)