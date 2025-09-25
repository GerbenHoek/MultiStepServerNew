module Utils.Num2Frac 
   ( Frac
   , numerator
   , denominator
   , toFrac) where 

import qualified Data.Ratio as R

data Frac a =  a :/: a
   deriving (Eq, Ord, Show)

instance Num a => Num (Frac a) where 
   (a:/:b) + (c:/:d) = (a*d + b*c):/:(b*d) 
   (a:/:b) * (c:/:d) = (a*c):/:(b*d)
   negate (a :/: b) = (negate a) :/: b 
   signum (a :/: b) = (signum a) :/: (signum b)
   abs (a :/: b) = (abs a) :/: (abs b)
   fromInteger n = (fromInteger n) :/: 1

instance Num a => Fractional (Frac a) where 
   fromRational r = (fromInteger $ R.numerator r) :/: (fromInteger $ R.denominator r)
   recip (a :/: b) = b :/: a

numerator :: Frac a -> a
numerator (a:/:_) = a

denominator :: Frac a -> a
denominator (_:/:b) = b

toFrac :: Num a => a -> a -> Frac a
toFrac a b = a :/: b
