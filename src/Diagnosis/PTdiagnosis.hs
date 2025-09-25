module Diagnosis.PTdiagnosis (ptDiagnosis) where 
import FSMMaker.StrategyBas (Strat)
import FSMMaker.FSM (fsm)
import Regulation.Tracer (element, lift2tracer)
import Regulation.Search (derivations)
import Diagnosis.RelationTree (TreeTracer, RelationTree, apply)

ptDiagnosis
  :: Ord a =>
     RelationTree a
     -> Strat a
     -> a
     -> TreeTracer a
ptDiagnosis t s a = 
   apply t $ map (element . fst) $ derivations fsm' a 
      where fsm' = fsm $ fmap (lift2tracer (const False)) s