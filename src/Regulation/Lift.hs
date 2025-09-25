module Regulation.Lift 
   ((>>>)
   , Lift
   , lift 
   , applyLift
   , makeLift
   , Context
   , listContext
   , head2list
   , single2list
   , elem2list
   , list2set
   , set2list
   , elem2set
   , single2set
   , modify
   , LblLift (getCon, lbl)
   , lblLift
   , LblContext) where 
import Prelude hiding (id, (.))
import qualified Prelude as P (id, (.))
import Control.Category 
import Data.Set (Set)
import qualified Data.Set as Set

data Lift a b = LFT {applyLift :: (b -> [(a, a -> b)])}
type Context a = Lift a a 

lift :: (b -> [(a, a -> b)]) -> Lift a b 
lift = LFT

makeLift :: (a -> b) -> (b -> a) -> Lift a b
makeLift a2b b2a = LFT $ \b -> [(b2a b, a2b)] 

instance Category Lift where 
   lbc . lab = 
      LFT $ \c -> 
         [ (a, fbc P.. fab) 
         | (b, fbc) <- (applyLift lbc) c
         , (a, fab) <- (applyLift lab) b]
   id = LFT $ \a -> [(a, P.id)]

listContext :: Context [a]
listContext = LFT $ listContext' P.id 
   where listContext' _ [] = []
         listContext' f (y:ys) = (y:ys, f):listContext' g ys  
            where g ys' = f(y:ys')

head2list :: Lift a [a]
head2list = LFT $ \as -> case as of 
   x:xs -> [(x, (:xs))] 
   _ -> []

list2set :: Ord a => Lift [a] (Set a)
list2set = makeLift Set.fromList Set.toList

set2list :: Ord a => Lift (Set a) [a] 
set2list = makeLift Set.toList Set.fromList

elem2list :: Lift a [a]
elem2list = head2list >>> listContext

elem2set :: Ord a => Lift a (Set a)
elem2set = elem2list >>> list2set

single2list :: Context [a]
single2list = single2list' >>> listContext
   where single2list' = LFT $ \as -> case as of 
            x:xs -> [([x], (++ xs))] 
            _ -> []

single2set :: Ord a => Context (Set a)
single2set = 
   set2list >>> single2list >>> list2set

modify :: Lift a b -> (a -> [a]) -> b -> [b]
modify a2b f = \b -> concat [map g $ f a| (a, g) <- applyLift a2b b] 

data LblLift a b = LC { getCon :: Lift a b
                      , lbl    :: [String]}

type LblContext a = LblLift a a 

lblLift :: Lift a b -> String -> LblLift a b
lblLift l = LC l . (:[])

instance Category LblLift where 
   (LC b t) . (LC a s) = LC (b . a) (t++s) 
   id = LC id []

   