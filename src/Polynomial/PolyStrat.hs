module Polynomial.PolyStrat where 
import Regulation.Search
import Regulation.Focus 
import Regulation.Lift ((>>>))
import qualified Regulation.Tracer as T 
import FSMMaker.Rules (path, Rule, makeMinor, ruleName, applyRule) 
import FSMMaker.StrategyBas hiding (applyAll)
--
import qualified FSMMaker.StrategyBas as SB (applyAll, shortestDerivation)
--
import FSMMaker.FSM (fsm)

import Polynomial.PolyLift 
import Polynomial.PolyRules

-- imports for distset
import Polynomial.ExprPoly hiding (rebracket, discriminant)
import qualified Regulation.Tracer as T
import SymbolRoot.ComplexRoot hiding (eval)

distSet :: OrList String ComRoot -> [OrList String ComRoot]
distSet p = 
   map (pqs . top . T.element . fst) $
   derivations fsm' $ embed $ PQS p
   where fsm' = fsm $ 
                fmap T.neutral2tracer distributeStrat


discSet :: OrList String ComRoot -> [[OrList String ComRoot]]
discSet = map (discriminants . pqs . top) . 
   applyRule (collapse s) . embed . PQS 
   where 
      s = --repeatS 
          focus e2s (try (R derive2zero))
          .*. 
          try (R eval) 
          .*. 
          option (focus e2s ((R factorLead <|> R factorDivisor)  
                              .*. repeatS (R divideByFirst)))

tt :: OrList String ComRoot
tt = list2or [3*var"x"^2+6*var "x" - 2:=: 0]

tt2 :: OrList String ComRoot
tt2 = list2or [var "D" :=: 6^2-4*3*(-2)]

evalAndZero :: Strat (Focus (PQS ComRoot))
evalAndZero = 
   try (R eval) 
   .*. try (focus e2s (R derive2zero)) 

leaveConstant :: Strat (Focus (PQS ComRoot))
leaveConstant =  
   try (focus e2s (R $ makeMinor derive2zero))
   .*. try (R evalMinor)
   .*. try (focus e2s (R transportConstant))

distributeStrat :: Strat (Focus (PQS ComRoot)) 
distributeStrat = 
   repeatS (focus (x2x >>> x2s) (R workoutSquare))
   .*. repeat1S  (focus (x2x >>> x2s) (R distribute) .*. R nubSub)
   .*. try (R rebracket)
   .*. try (R eval)
   .*. R wait
   .*. R nubSub
   .*. R distributePath

factorStrat :: Strat (Focus (PQS ComRoot))
factorStrat =  
      (evalAndZero
      .*. focus (tlf >>> tlt >>> x2s) (try (R workoutSquare)) 
      .*. focus e2s (focus x2e (R permutateFactors 
                                .*. R factor))
      .*. R evalMinor
      .*. R wait
      .*. R nubSub
      .*. try (focus (tlt >>> tlf >>> x2s) (R distribute))
      .*. try (R rebracket)  
      .*. R eval                 
      .*. R nillProduct
      <|> 
      focus (tlf >>> tlt >>> x2s) (try (R workoutSquare))
      .*. focus x2s (R permutateFactors) .*. R factorDirect)
      .*. try distributeStrat
      .*. repeatS (focus s2s (R removeDegen))
      .*. R doubleFactorPath

linSquareStrat :: Strat (Focus (PQS ComRoot))
linSquareStrat = 
                 leaveConstant
                 .*. focus e2s (try (R transportConstant) 
                 .*. option (R divideByFirst))
                 .*. R linSquare
                 .*. R linSquarePath

factorThreeTermStrat :: Strat (Focus (PQS ComRoot))
factorThreeTermStrat =
         evalAndZero 
         .*.  focus e2s ( option (R factorLead <|> R factorDivisor) 
                          .*. option (R divideByFirst) 
                          .*. focus (tlf >>> x2e) ( R factorThreeTerm 
                                                    <|> R factorTwoTerm)) 
         .*. R nubSub
         .*. nillProductStrat                 
         .*. (R hasZeroRoot 
             .*. R factorTwoTermPath 
             <|>
             notS (R hasZeroRoot) 
             .*. R factorThreeTermPath)

completeSquareStrat :: Strat (Focus (PQS ComRoot))
completeSquareStrat = 
   evalAndZero 
   .*. focus e2s (try (R factorLead) .*. option (R divideByFirst) 
   .*. focus (tlf >>> x2e) (R completeSquare) 
   .*. option (focus (tlt >>> x2e) (R distribute))
   .*. try (R divideByFirst))
   .*. R completeSquarePath
   .*. linSquareStrat
   
nillProductStrat :: Strat (Focus (PQS ComRoot))
nillProductStrat = 
   evalAndZero
   .*. focus e2s (try (R divideByFirst 
                       <|> focus x2e (R $ makeMinor permutateFactors) 
                           .*. focus (tlt >>> x2e) (R distributeConst)))
   .*. try (R rebracket)
   .*. R nillProduct
   .*. R nillProductPath

abcStrat :: Strat (Focus (PQS ComRoot))
abcStrat = 
   option (focus e2s ((R factorLead <|> R factorDivisor) .*. repeatS (R divideByFirst)))
   .*. focus e2s (R discriminant)
   .*. R abcFromDisc  
   .*. try (R simplifyAllRoots) 
   .*. R abcPath

linStrat :: Strat (Focus (PQS ComRoot))
linStrat = 
    R allLin
    .*. try distributeStrat
    .*. repeatS (focus e2s (R $ makeMinor sortLin))
    .*. R showStep
    .*. repeatS (focus e2s (R $ makeMinor divideByFirst))
    .*. R showStep
    .*. try (R simplifyAllRoots)

polyStrat :: Strat (Focus (PQS ComRoot))
polyStrat = 
  linStrat |> 
      (factorStrat' <|> nillProductStrat' <|> linSquareStrat' <|> factorThreeTermStrat') 
       .*. linStrat
           |> try distributeStrat 
           .*. (((evalAndZero .*. factorThreeTermStrat' <|> leaveConstant .*. linSquareStrat') 
              .*. linStrat) |> evalAndZero .*. (abcStrat <|> completeSquareStrat .*. linStrat))
   where factorStrat'          = (testStrat factorStrat) 
                                 .*. factorStrat 
         nillProductStrat'     = (testStrat nillProductStrat)
                                 .*. nillProductStrat
         linSquareStrat'       = (testStrat linSquareStrat) 
                                 .*. linSquareStrat 
         factorThreeTermStrat' = (testStrat factorThreeTermStrat) 
                                 .*. (factorThreeTermStrat <|> completeSquareStrat)


isStrat :: Rule a -> Bool
isStrat r = 
   (ruleName r) `elem` map ruleName 
      [ distributePath
      , dividePath
      , linSquarePath
      , factorTwoTermPath 
      , factorThreeTermPath
      , doubleFactorPath
      , nillProductPath
      , completeSquarePath
      , abcPath
      , linPath] 

tester a = 
   --reverse $ 
   --map (trace . fst) $
   map (top . T.element) $ 
   SB.shortestDerivation (fmap T.neutral2tracer polyStrat) $ 
   T.embed $ embed $ PQS a

tester2 a = 
   reverse $ 
   --map (trace . fst) $
   map (top . T.element . fst) $ 
   derivations (fsm $ fmap T.neutral2tracer $ polyStrat) $ 
   embed $ PQS a



--embed $ PQS $ 
--  list2or [(var "x" -1).^.2 :=: (var "x" -1)*(var "x" -2)]
-- [var "x" :=: N (sqRoot 3)]
--   [var "x".^.2 + 5*var "x" + 1 :=: 0] 
 --[(var "x" -3)*(var "x" +6) :=: (var "x" -3)*(3*var "x" +8)]
-- [-3 + var "x" :=: 0, -2 + (-2) * var "x" :=: 0]
--  [6*(var "x" +6).^.2 -9 :=: 0]
--  [var "x" .^. 2 + 4 * var "x" + 1 :=: 0]