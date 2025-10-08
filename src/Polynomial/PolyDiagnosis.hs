module Polynomial.PolyDiagnosis where
import Regulation.Search 
import Regulation.Tracer hiding (embed)
import Regulation.Focus
import Diagnosis.RelationTree (apply, TreeTracer, unSat, sat)
import Diagnosis.MBTdiagnosis (countBuggy)
import Polynomial.ExprPoly
import Polynomial.PolyLift (PQS (..))
import Polynomial.PolyStrat hiding (tester)
import Polynomial.BuggyStrat
import Polynomial.PolyRelations
import Polynomial.Ready
import FSMMaker.FSM 
import Utils.Utils (setNub)
import Communication.ParseExpr (parse2or)
import SymbolRoot.ComplexRoot hiding (simplify)
import qualified SymbolRoot.ComplexRoot as CR (simplify)
import Data.Either (isRight)
import Data.List (intersperse)
import Utils.Utils (replace)
import Polynomial.MBTfeedback (getMBTFeedback)
import Polynomial.PTfeedback (getPTFeedback)
import Polynomial.PTHint (getPTHint)

polyDiagnosis
  :: OrListC
     -> OrListC
     -> Either [[String]] (TreeTracer OrListC)
polyDiagnosis a b = 
   if s a == s b && s b /= []
      || (s b) `elem` (disc a)
   then Right $ apply (polyTree a b) 
              $ map getElement pts
   else Left  $ nubSubList
              -- $ setNub $ map setNub  
              $ concatMap trace 
              $ filter ((== (s' b)) . s' .  getElement) 
             pts
   where s = map setNubOr . solveDegLT2 
         s' = map sortOr . solveDegLT2 
         disc = map (map setNubOr) . discSet
         getElement = pqs . top . element
         pts = map fst $
               (if s a == s b && s b /= []
                || (s b) `elem` (disc a)
                  then derivations (fsm $ fmap neutral2tracer polyStrat)  
                  else applyAll (fsm $ fmap (countBuggy 3 . buggy2tracer) buggyStrat)) $ 
               embed $ PQS a

onStrat :: OrListC -> OrListC -> Bool
onStrat a b = either (const False) (null . unSat) $ polyDiagnosis a b 

traceStrategy :: OrListC -> [[String]]
traceStrategy a = 
   map reverse $
   nubSubList $
   concatMap (trace . fst) $
   applyAll (fsm $ fmap (lift2tracer f) polyStrat) $ embed $ PQS a
   where f = isStrat

tester a = 
   reverse $ 
   map pqs $
   map (top . element . fst) $ 
   applyAll (fsm $ fmap neutral2tracer abcStratB) $ 
   embed $ PQS a

tta :: OrListC
tta = list2or --[3*(var "x" -1)*(var "x" -2) :=: 0]
   --[(-var "x" +4)*(2*var "x" +7) :=: (-var "x" +4)*(-5*var "x" +9) ]
              --[var "x" :=: 2, 2:=:3]
      --[var "x".^. 2 - 8*var "x" :=: 0]
      --[(var "x"  + 11) * var "x" :=: -18]
      --[3*var "x" .^. 2 -2 * var "x" :=:0]
      [3*var "x".^.2 - 2*var "x" - 2 :=:0]


fl :: [OrListC] -> [OrListC]
fl = filter g
   where g = ((Var "x" :=: 0) `elem`) . or2list

ttb :: OrListC
ttb = list2or [var "D" :=: 2^2-4*3*(-2)]


--[var "x" ^ 2 - 2*var "x" - 1 - 9 :=: 0]
   --[var "x" .^. 2 - 4 * var "x" + 1 :=: 0]   
   --[var "x" + 11 :=: -18,  var "x" :=: -18 ]
     --[ 8*(5*(N $ sqRoot 3) * var "x" - 8*(N $ sqRoot 3)) :=: 8*(N $ sqRoot 2)
     --, 8*(5*(N $ sqRoot 3) * var "x" - 8*(N $ sqRoot 3)) :=: 8*(N $ -sqRoot 2)]

isDisc :: OrListC -> Bool
isDisc p = case or2list p of 
   [Var "D" :=: _] -> True  
   _ -> False

parseAll :: [String] -> Either String [OrListC]
parseAll = traverse parse2or 

listDiagnosis' :: [OrListC] -> (String, String, Integer)
listDiagnosis' []  = ("not enough input", "", 2) 
listDiagnosis' [s] = ("not enough input", "", 2)
listDiagnosis' (a:t:ts) = 
      if isDisc t then listDiagnosis' (a:ts) else    
      case polyDiagnosis t a of
         Left mbt -> (getMBTFeedback mbt, "", 2)
         Right pt -> 
            case unSat pt of 
               [] -> ("Dit is een verwachte strategie stap.", "", 0)
               un -> ( getPTFeedback (sat pt) un
                     , getPTHint $ concat $ traceStrategy t, 1)

listDiagnosis :: [OrListC] -> (String, String, Integer)
listDiagnosis (x:y:z:xs)
   | onStrat z y = listDiagnosis' (x:y:z:xs) 
   | onStrat y x = listDiagnosis' (x:y:z:xs) 
   | otherwise   = listDiagnosis' (x:z:xs) 
listDiagnosis xs = listDiagnosis' xs


readyDiagnosis :: [OrListC] -> (String, String, Integer)
readyDiagnosis ps = let (_, _, n) = listDiagnosis ps in
   if n `elem` [0,1] ++ [5]
   then if snd (ready ps) == 2 then listDiagnosis ps
        else let (s, n) = ready ps in (s, "", n) 
   else listDiagnosis ps 

stringDiagnosis :: [String] -> (String, String, Integer)
stringDiagnosis ss = case parseAll ss of 
   Left msg -> (msg, "", 3)
   Right ps -> readyDiagnosis ps