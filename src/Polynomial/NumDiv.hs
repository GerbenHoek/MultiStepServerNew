module Polynomial.NumDiv
 ( pDiv
 , quotient
 , remainder
 , isDivBy
 , isDivByAll
 , divideOut
 , divideOutAll
 , euclid
 , gcdp
 , resultant
 , nonCommonFactor
 , sturm 
 , nrZero
 , test) where 
import Polynomial.SinglePoly

pDiv
  :: (Ord x, Num a, Eq a) =>
     SinglePoly x a
     -> SinglePoly x a
     -> (SinglePoly x a, SinglePoly x a)
pDiv a b = if degree b == 0 then (a, 0) 
   else pdiv a (abs b) 0 
   where 
      pdiv _ 0 _ = (0,0)
      pdiv 0 _ d = (d,0)
      pdiv p q d =
         let lp = leadCoeff p
             lq = leadCoeff q
             n = degree p 
             k = degree q
             x = getVar p 
             quo = constant lp * mvar x ^ (n - k)
             r = constant lq * deleteLead p - quo * deleteLead q 
         in if n >= k 
            then pdiv r q (quo + constant lq * d) 
            else (d, p)

quotient 
   :: (Ord x, Num a, Eq a) =>
      SinglePoly x a
      -> SinglePoly x a
      -> SinglePoly x a
quotient a b = fst $ pDiv a b

remainder 
   :: (Ord x, Num a, Eq a) =>
      SinglePoly x a
      -> SinglePoly x a
      -> SinglePoly x a
remainder a b = snd $ pDiv a b

isDivBy 
   :: (Ord x, Num a, Eq a) =>
      SinglePoly x a
      -> SinglePoly x a
      -> Bool
isDivBy a b = remainder a b == 0 

isDivByAll 
   :: (Ord x, Num a, Eq a) =>
      SinglePoly x a
      -> [SinglePoly x a]
      -> Bool
isDivByAll a = 
   all (a `isDivBy`) 

divideOut 
   :: (Ord x, Num a, Eq a) =>
      SinglePoly x a
      -> SinglePoly x a
      -> SinglePoly x a
divideOut a b = 
   let (q, r) = pDiv a b in
   if r/=0 then a else divideOut q b

divideOutAll 
   :: (Ord x, Num a, Eq a) =>
      SinglePoly x a
      -> [SinglePoly x a]
      -> SinglePoly x a
divideOutAll a bs = 
   foldr (flip divideOut) a bs

euclid
  :: (Ord x, Num a, Eq a) =>
     SinglePoly x a
     -> SinglePoly x a
     -> [(SinglePoly x a)]
euclid a b = a:b:chain a b  
   where 
      chain r0 r1 = 
         if r1 /= 0  
         then r2:chain r1 r2
         else []
         where 
            r2 = remainder r0 r1

gcdp 
   :: (Ord x, Num a, Eq a) =>
     SinglePoly x a
     -> SinglePoly x a
     -> SinglePoly x a
gcdp a b = last $ init $ euclid a b

resultant :: (Ord x, Num a, Eq a) =>
     SinglePoly x a
     -> SinglePoly x a
     -> SinglePoly x a
resultant 0 _ = 0
resultant _ 0 = error "divide by zero"   
resultant a b = 
   if degree d == 0 
   then a else resultant (a `quotient` d) b
   where d = gcdp a b

nonCommonFactor 
   :: (Ord x, Num a, Eq a) =>
      SinglePoly x a
      -> SinglePoly x a
      -> SinglePoly x a
nonCommonFactor a b = a `quotient` (gcdp a b) 

sturm
  :: (Ord x, Num a, Eq a) =>
     SinglePoly x a
     -> [(SinglePoly x a)]
sturm a = a:da:chain a da  
   where 
      da = derivative a 
      chain r0 r1 = 
         if degree r1 > 0  
         then r2:chain r1 r2
         else []
         where 
            r2 = negate $ remainder r0 r1

nrZero 
   :: (Ord x, Num a, Eq a) =>
      SinglePoly x a -> Maybe Int
nrZero 0 = Nothing
nrZero a = Just $ change (map signLow sturmChain) - change (map signHigh sturmChain)
      where sturmChain = sturm a
            signLow b = (-1) ^ (degree b) * (signum b)
            signHigh b = signum b
            change []       = 0
            change [_]      = 0 
            change (x:y:xs) = if signum x /= signum y 
                              then change (y:xs) + 1
                              else change (y:xs)

test 
   :: (Ord x, Num a, Eq a) =>
      SinglePoly x a
      -> SinglePoly x a
      -> Maybe (Ordering)
test 0 0 = Just EQ
test 0 _ = Nothing
test _ 0 = Nothing
test a b 
   | t1 && t2 = Just EQ
   | t1       = Just GT
   | t2       = Just LT
   | otherwise = Nothing 
   where t1 = nrZero (resultant b a) == Just 0 
         t2 = nrZero (resultant a b) == Just 0 