module Regulation.Search 
   ( nubSub
   , wait
   , applyAll
   , derivations) where 
import Utils.Utils (copy)
import FSMMaker.Rules (Rule, applyRule, ruleName, minorRule, isMinor)
import FSMMaker.FSM (FSM, firsts, start, accept)
import Regulation.Tracer (Tracer (..), embed)
import qualified Regulation.Tracer as T (nubSub)

nubSub :: Rule a 
nubSub = minorRule "nubSub" (:[])

wait :: Rule a 
wait = minorRule "wait" (:[])
      
data WorkHorse a = WH { current  :: [(Tracer a, Int)]
                      , next     :: [(Tracer a, Int)]
                      , points   :: [(Tracer a, Int)]
                      , waiting  :: [(Tracer a, Int)]
                      , doNub    :: Bool}

apply :: FSM (Rule a) -> (a, Int) -> [((a, Int), Maybe (Rule a), Bool)] 
apply ms (a, n) = do
   (mr, n') <- firsts ms n 
   a'       <- maybe [a] (flip applyRule a) mr
   return ((a', n'), mr, n' `elem` (accept ms))

insert :: 
   [((Tracer a, Int), Maybe (Rule (Tracer a)), Bool)] 
      -> WorkHorse a -> WorkHorse a
insert ts wh = foldr g wh ts 
   where isWait = maybe False ((== "wait").ruleName) 
         isNub  = maybe False ((== "nubSub").ruleName)
         isMin  = maybe True isMinor 
         g (t, mr, acc) wh' 
            | acc       = (copy wh') {points  = t:points wh'} 
            | isWait mr = (copy wh') {waiting = t:waiting wh'}
            | isNub  mr = (copy wh') {next    = t:next wh', doNub = True}
            | isMin  mr = (copy wh') {next    = t:next wh'}
            | otherwise = (copy wh') {next    = t:next wh', points = t:points wh'}

step
  :: Ord a => 
     FSM (Rule (Tracer a)) -> WorkHorse a -> WorkHorse a
step ms wh = case wh of 
   WH [] [] _ [] _ -> wh 
   WH [] [] _ _ _  -> (copy wh) {current = waiting wh, waiting = []}  
   WH [] _ _ _ _  | doNub wh  -> (copy wh) {current = nbs (next wh), next = []} 
                  | otherwise -> (copy wh) {current = next wh, next = []} 
   WH (w:ws) _ _ _ _ -> (insert (apply ms w) wh){current = ws}
   where nbs = map unwrap . T.nubSub . map wrap
         unwrap (T (a, n) s) = (T a s, n)
         wrap (T a s, n)     = (T (a, n) s)

steps
  :: Ord a => 
     FSM (Rule (Tracer a)) -> WorkHorse a -> WorkHorse a
steps ms wh = case wh of 
   WH [] [] _ [] _ -> wh 
   _ -> steps ms (step ms wh) 

derivations'
  :: Ord a => 
     FSM (Rule (Tracer a)) -> Tracer a -> [(Tracer a, Int)]
derivations' sm t = 
   (t, start sm):(points $ steps sm (WH [(t, start sm)] [] [] [] False))  

derivations 
   :: Ord a =>
      FSM (Rule (Tracer a)) -> a -> [(Tracer a, Int)]
derivations sm = derivations' sm . embed 

applyAll'
  :: Ord a => 
     FSM (Rule (Tracer a)) -> Tracer a -> [(Tracer a, Int)]
applyAll' sm = filter ((`elem` (accept sm)) . snd) . derivations' sm 

applyAll
   :: Ord a =>
      FSM (Rule (Tracer a)) -> a -> [(Tracer a, Int)]
applyAll sm = applyAll' sm . embed 