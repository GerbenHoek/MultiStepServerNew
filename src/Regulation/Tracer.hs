module Regulation.Tracer 
   ( Tracer (..)
   , embed
   , lift2tracer
   , buggy2tracer
   , neutral2tracer
   , nubSub
   , nubSubList) where
import Data.Set (isSubsetOf, size)
import Data.List (nubBy, sortBy)
import qualified Data.Set as Set (fromList, toList)
import FSMMaker.Rules (Rule, isBuggy, ruleName, applyRule)
import qualified Data.Map as Map (fromListWith, toList)

data Tracer a = T { element   :: a
                  , trace     :: [[String]]}
   deriving Show

embed :: a -> Tracer a
embed a = T a [[]] 

lift2tracer :: (Rule a -> Bool) -> Rule a -> Rule (Tracer a)
lift2tracer b r = r {applyRule = f}
   where 
      f t = [ t {element = a, trace = d} 
            | a <- applyRule r (element t)]   
         where d = if b r then map ((ruleName r):) $ trace t
                          else trace t

buggy2tracer :: Rule a -> Rule (Tracer a)
buggy2tracer = lift2tracer isBuggy

neutral2tracer :: Rule a -> Rule (Tracer a)
neutral2tracer = lift2tracer (const False)

nubSubList :: Ord a => [[a]] -> [[a]]
nubSubList = map Set.toList 
             . nubBy isSubsetOf 
             . sortBy s 
             . map Set.fromList 
   where s a b = compare (size a) (size b) 

nubSub :: Ord a => [Tracer a] -> [Tracer a]
nubSub = map tup' . Map.toList . Map.fromListWith f . map tup
   where 
      f x y = nubSubList $ 
         x ++ y
      tup (T a t) = (a, t)
      tup'(a, t) = T a t      