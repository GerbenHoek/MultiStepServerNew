module Diagnosis.RelationTree 
   ( TreeTracer (..)
   , embed
   , Test(..)
   , test
   , binary
   , RelationTree (A)
   , apply
   , stop
   , (.>>)
   , (|>)) where 

data TreeTracer a = T { elements :: [a] 
                      , sat      :: [String] 
                      , unSat    :: [String]}
   deriving Show

instance Functor TreeTracer where
   fmap g (T as st un)  = T (map g as) st un 

embed :: [a] -> TreeTracer a
embed as = T as [] []

data Test a = TST 
        { applyTest :: TreeTracer a -> TreeTracer a
        , testName :: String}

data RelationTree a = RelationTree a :>> RelationTree a
                    | RelationTree a :|> RelationTree a
                    | A (Test a)
                    | Stop

stop :: RelationTree a 
stop = Stop

apply' :: RelationTree a -> TreeTracer a -> TreeTracer a 
apply' (t :>> u) a = let a' = apply' t a in  
   if null $ elements a' 
   then a' 
   else apply' u a'
apply' (t :|> u) a  = let a' = apply' t a in 
   if null $ elements a' 
   then apply' u $ T (elements a) (sat a') (unSat a')
   else a'
apply' (A f) a = applyTest f a  
apply' Stop a = a {elements = []}

apply :: RelationTree a -> [a] -> TreeTracer a
apply t as = apply' t (T as [] [])

(.>>), (|>) :: RelationTree a -> RelationTree a -> RelationTree a
(t :>> u) .>> v = t .>> (u .>> v)
t .>> u = t :>> u
infixl 7 .>>

(t :|> u) |> v = t |> (u |> v) 
t |> u = t :|> u
infixl 6 |>

test :: (a -> Bool) -> String -> Test a
test p s  = TST 
   (\(T as st un) -> let as' = filter p as in
      if null as'
      then T [] st (s:un) 
      else T as' (s:st) un) s

binary :: Bool -> String -> Test a
binary p s = TST (\(T as st un) -> 
   if p  
   then T as (s:st) un
   else T [] st (s:un)) s    