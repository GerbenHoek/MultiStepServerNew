module Utils.ComplexNum 
   (re, im, rep, imp, Complex) where 

data Complex a = CP a a
   deriving (Eq, Ord)

instance (Show a, Num a, Eq a) => Show (Complex a) where 
   show (CP a b) 
      | a == 0 && b == 0 = show 0
      | a == 0 = show b ++ " i"
      | b == 0 = show a
      | otherwise = br $ show a ++ " + " ++ show b ++ " i" 
      where br s = "(" ++ s ++ ")"

re, im :: Num a => a -> Complex a
re a = CP a 0
im b = CP 0 b

rep, imp :: Complex a -> a
rep (CP a _) = a
imp (CP _ b) = b 

instance Functor Complex where 
   fmap g (CP a b) = CP (g a) (g b)

instance Num a => Num (Complex a) where 
   CP a b + CP a' b' = CP (a + a') (b + b')
   CP a b - CP a' b' = CP (a - a') (b - b')
   CP a b * CP a' b' = CP (a * a' - b * b') (a * b' + a' * b)
   abs (CP a b) = CP (signum a * a) (signum a * b) 
   signum (CP a _) = CP (signum a) 0 
   fromInteger n = CP (fromInteger n) 0

instance Fractional a => Fractional (Complex a) where 
   recip (CP a b) = CP (a/r) (-b/r) 
      where r = a^2 + b^2
   fromRational q = CP (fromRational q) 0