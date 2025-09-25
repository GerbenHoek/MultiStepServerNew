module Regulation.Focus 
   ( Focus (target, back, ids)
   , top 
   , embed
   , lift2focus
   , focus)
where
import Regulation.Lift (applyLift, LblContext, getCon, lbl)
import FSMMaker.Rules (Rule, minorRule, applyRule)
import FSMMaker.StrategyBas 

data Focus a = F { target :: a 
                 , back   :: [a -> a]
                 , ids    :: [([String], Int)]}

top :: Focus a -> a
top x = (foldr (flip (.)) id (back x)) $ target x

instance Eq a => Eq (Focus a) where
   x == y = 
      (top x, target x, ids x) == (top y, target y, ids y)

instance Ord a => Ord (Focus a) where 
   compare x y = compare (top x, target x, ids x) 
                         (top y, target y, ids y)

instance Show a => Show (Focus a) where 
   show x = show (top x, target x, ids x)

embed :: a -> Focus a 
embed a = F a [id] []

focusR :: LblContext a -> Rule (Focus a)
focusR ct = minorRule "focus" g 
   where 
      g (F x fs ls) = do 
         ((y, f), n) <- zip (applyLift (getCon ct) x) [0..]
         return (F y (f:fs) ((lbl ct, n):ls)) 

unfocusR :: Rule (Focus a)
unfocusR = minorRule "unfocus" g 
   where g (F x (f:fs) (_:ns)) = [F (f x) fs ns]
         g _   = error "already focussed out"

lift2focus :: Rule a -> Rule (Focus a)
lift2focus r = r {applyRule = g}
   where g (F x fs ls) = [F y fs ls | y <- applyRule r x]

focus :: LblContext a -> Strat (Focus a) -> Strat (Focus a)
focus ct s = R (focusR ct) .*. s .*. R unfocusR