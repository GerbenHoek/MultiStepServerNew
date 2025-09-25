module Polynomial.LoopDiv 
   (inverseOver, modulo) where 
import Polynomial.SinglePoly

pDiv
  :: (Ord x, Num a, Eq a) =>
     (a -> a -> a)
     -> SinglePoly x a
     -> SinglePoly x a
     -> (SinglePoly x a, SinglePoly x a)
pDiv divide a b = pdiv a b 0
   where 
      pdiv _ 0 _ = (0,0)
      pdiv 0 _ d = (d,0)
      pdiv p q d =
         let lp = leadCoeff p
             lq = leadCoeff q
             n = degree p 
             k = degree q
             x = getVar p 
             quo = constant (divide lp lq) * mvar x ^ (n - k)
             r = (deleteLead p) - deleteLead (quo * q) 
         in if n >= k 
            then pdiv r q (quo + d)
            else (d, p)

modulo
  :: (Ord x, Num a, Eq a) =>
     (a -> a -> a) -> SinglePoly x a -> SinglePoly x a -> SinglePoly x a
modulo divide a b = if degree b == 0 
                    then a 
                    else let (_,r) = pDiv divide a b in r  

exEuclid
  :: (Ord x, Num a, Eq a) =>
     (a -> a -> a)
     -> SinglePoly x a
     -> SinglePoly x a
     -> [(SinglePoly x a, SinglePoly x a, SinglePoly x a, SinglePoly x a)]
exEuclid divide a b = (1,a,0,1):(1,b,1,0):chain (a, b) (1, 0) (0, 1) 
   where 
      chain (r0,r1) (s0,s1) (t0,t1) = 
         if r1 /= 0  
         then (q,r2,s2,t2):chain (r1, r2) (s1, s2) (t1, t2)
         else []
         where 
            (q,r2) = pDiv divide r0 r1
            s2 = s0 - q * s1
            t2 = t0 - q * t1

inverseOver
  :: (Ord x, Num a, Eq a) =>
     (a -> a -> a) -> SinglePoly x a -> SinglePoly x a -> SinglePoly x a
inverseOver divide a b = s * constant (divide 1 (constantVal r)) 
   where (_,r,s,_) = last $ init $ exEuclid divide a b