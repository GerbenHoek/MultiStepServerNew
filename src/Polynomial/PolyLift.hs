module Polynomial.PolyLift 
   ( PQS(..)
   , PEX
   , PEQ  
   , rPEX
   , rPEQ
   , rPQS
   , x2x
   , x2e
   , e2s
   , x2s
   , s2s
   , lhs
   , rhs
   , tlt 
   , tlf
   , dct
   , dcf) where 
import Polynomial.ExprPoly 
   ( ExprPoly, PolyEq (..), OrList (..)
   , contextP, terms2list, factors2list, or2list, list2or)
import Regulation.Lift 
   ( Lift, Context, LblLift, LblContext, lift, lblLift, makeLift, modify
   , elem2list, listContext, elem2set, single2list, (>>>))
import FSMMaker.Rules (Rule, applyRule)
--import Regulation.Focus (LblContext, lblContext)

data PQS a = PQS {pqs :: OrList String a} 
           | PEQ {peq :: PolyEq String a}
           | PEX {pex :: ExprPoly String a }
   deriving (Eq, Ord)

type PEX a = PQS a
type PEQ a = PQS a

instance (Show a, Eq a, Num a) => Show (PQS a) where
   show (PQS a) = show a
   show (PEQ a) = show a
   show (PEX a) = show a

rPEX :: Rule (ExprPoly String a) -> Rule (PEX a)
rPEX r = r {applyRule = f}
   where f = modify (makeLift PEX pex) (applyRule r) 

rPEQ :: Rule (PolyEq String a) -> Rule (PEQ a)
rPEQ r = r {applyRule = f}
   where f = modify (makeLift PEQ peq) (applyRule r)

rPQS :: Rule (OrList String a) -> Rule (PQS a)
rPQS r = r {applyRule = f}
   where f = modify (makeLift PQS pqs) (applyRule r)

exp2eq :: Lift (ExprPoly x a) (PolyEq x a)
exp2eq  = lift $ \(p :=: q) ->
   [(p, \p'-> p' :=: q),(q, \q'-> p :=: q')] 

lhs2eq :: Lift (ExprPoly x a) (PolyEq x a)
lhs2eq = lift $ \(p :=: q) -> [(p, \p'-> p' :=: q)] 

rhs2eq :: Lift (ExprPoly x a) (PolyEq x a)
rhs2eq = lift $ \(p :=: q) -> [(q, \q'-> p :=: q')] 

eq2or :: Ord x => Lift (PolyEq x a) (OrList x a)
eq2or = elem2list >>> makeLift list2or or2list

or2or :: Ord x => Context (OrList x a)
or2or = makeLift or2list list2or
        >>> single2list 
        >>> makeLift list2or or2list

x2x :: LblContext (PEX a)
x2x = lblLift ct "x2x"
   where ct = makeLift pex PEX
              >>> (lift contextP) 
              >>> makeLift PEX pex

x2e :: LblLift (PEX a) (PQS a)
x2e = lblLift ct "x2e"
   where ct = makeLift pex PEX 
              >>> exp2eq 
              >>> makeLift PEQ peq

e2s :: LblLift (PEQ a) (PQS a)
e2s = lblLift ct "e2s"
   where ct = makeLift peq PEQ 
              >>> eq2or 
              >>> makeLift PQS pqs

x2s :: LblLift (PEQ a) (PQS a)
x2s = x2e >>> e2s

s2s :: LblContext (PQS a)
s2s = lblLift ct "s2s"
   where ct = makeLift pqs PQS 
              >>> or2or 
              >>> makeLift PQS pqs

lhs :: LblContext (PQS a)
lhs = lblLift ct "lhs"
   where ct = makeLift pex PEX 
              >>> lhs2eq
              >>> makeLift PEQ peq

rhs :: LblContext (PQS a)
rhs = lblLift ct "rhs"
   where ct = makeLift pex PEX 
              >>> rhs2eq
              >>> makeLift PEQ peq

tlt :: 
   (Num a, Ord a) => LblContext (PEX a)
tlt = lblLift ct "tlt"
   where ct = makeLift pex PEX 
              >>> elem2list 
              >>> l2t
              >>> makeLift PEX pex
         l2t = lift $ \p -> [(terms2list p, foldr1 (+))]

tlf :: 
   (Num a, Ord a) => LblContext (PEX a)
tlf = lblLift ct "tlf"
   where ct = makeLift pex PEX 
              >>> elem2list 
              >>> l2f
              >>> makeLift PEX pex
         l2f = lift $ \p -> [(factors2list p, foldr1 (*))]

dct ::
   (Num a, Ord a) => LblContext (PEX a)
dct = lblLift ct "dct"
   where ct = makeLift pex PEX
              >>> t2l 
              >>> listContext  
              >>> l2t
              >>> makeLift PEX pex
         t2l = lift $ \xs ->[(foldr1 (+) xs, terms2list)]   
         l2t = lift $ \p -> [(terms2list p, foldr1 (+))]

dcf ::
   (Num a, Ord a) => LblContext (PEX a)
dcf = lblLift ct "dcf"
   where ct = makeLift pex PEX
              >>> f2l 
              >>> listContext  
              >>> l2f
              >>> makeLift PEX pex
         f2l = lift $ \xs ->[(foldr1 (*) xs, factors2list)]   
         l2f = lift $ \p -> [(factors2list p, foldr1 (*))]