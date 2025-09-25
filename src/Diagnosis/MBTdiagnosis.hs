module Diagnosis.MBTdiagnosis where 
import FSMMaker.Rules (Rule, applyRule)
import FSMMaker.StrategyBas (Strat)
import FSMMaker.FSM (fsm)
import Regulation.Tracer (Tracer(..), buggy2tracer, nubSubList)
import Regulation.Search (applyAll)


countBuggy :: Int -> Rule (Tracer a) -> Rule (Tracer a)
countBuggy k r = r {applyRule = f} 
   where f t = if (maximum $ map length $ trace t) >= k 
               then []
               else applyRule r t 

mbtDiagnosis :: 
   Ord a => 
      (a -> a -> Bool) -> Strat a -> Int -> a -> a -> [[String]]
mbtDiagnosis eq s n a b = 
   nubSubList $
   concatMap trace $
   filter ((`eq` b) . element) $ map fst $ applyAll fsm' $ a 
      where fsm' = fsm $ fmap (countBuggy n) $ fmap buggy2tracer s