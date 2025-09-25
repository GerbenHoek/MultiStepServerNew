module Polynomial.PolyBuggys where 
import Polynomial.ExprPoly
import FSMMaker.Rules 
import Regulation.Focus 
import Polynomial.PolyLift
import Regulation.Lift (elem2list, applyLift) 
import qualified SymbolRoot.ExprRoot as ER (constant, eval)
import SymbolRoot.ComplexRoot  
import Utils.Utils(roundDown, roundUp, rounder)

negateTerm :: (Num a, Ord a) => Rule (Focus (PEX a))
negateTerm = lift2focus $ rPEX $ buggyRule "negate a term" f 
   where f p = [-p]

negateSolution :: (Num a, Ord a) => Rule (Focus (PEX a))
negateSolution = lift2focus $ rPEQ $ buggyRule "negate a term" f 
   where f p = case p of 
               Var "x" :=: N a | a /= 0 -> [Var "x" :=: N (-a)]
               _ -> []

forgetEquation :: Rule (Focus (PQS a)) 
forgetEquation = lift2focus $ rPQS $ buggyRule "forget an equation" f
   where 
      f p = case p of 
         OrList [x, y] -> [OrList [x], OrList [y]] 
         _ -> []

distributeB' :: (Num a, Ord a) => Rule (ExprPoly String a)
distributeB' = buggyRule "distribute, only multiply one of the terms" f 
   where 
      f p = case p of
           a :*: (b :*: c) 
              | not(isSum a) && isSum b -> map (*c) (g a b)
              | isSum a && not(isSum b) -> map (*c) (g b a)
             -- | isSum a && isSum b      -> map (*c) (g a b ++ g b a) 
           a :*: b 
              | not(isSum a) && isSum b -> g a b
              | isSum a && not(isSum b) -> g b a
             -- | isSum a && isSum b      -> g a b ++ g b a 
           _ -> []
      isSum (_ :+: _) = True
      isSum _         = False
      g x y = map (foldr1 (+)) [ gy (x * y') 
                               | (y', gy) <- applyLift elem2list 
                                             $ terms2list y]

distributeB :: (Num a, Ord a) => Rule (Focus (PEX a))
distributeB = lift2focus $ rPEX distributeB'

tt = (var "x" + 1) * (var "x".^.2 + 2* var "x" +1)  

workoutSquareB1 :: (Num a, Ord a) => Rule (Focus (PEX a))
workoutSquareB1 = lift2focus $ rPEX $ buggyRule "square of sum is sum of squares" f 
   where 
      f p = case p of 
          (a:+:b):^:2 -> [ a^2 + b^2]
          _ -> []

workoutSquareB2 :: (Num a, Ord a) => Rule (Focus (PEX a))
workoutSquareB2 = lift2focus $ rPEX $ buggyRule "square of sum is sum of squares factor" f 
   where 
      f p = case p of 
          (a :*: b :+: c):^:2 -> [ a * b^2 + c^2]
          (c :+: (a :*: b)):^:2 -> [ c^2 + a * b^2]
          _ -> []

workoutSquareB3 :: (Num a, Ord a) => Rule (Focus (PEX a))
workoutSquareB3 = lift2focus $ rPEX $ buggyRule "square of sum is sum of squares minus" f 
   where 
      f p = case p of 
          (a:+:b):^:2 -> [ a^2 - b^2]
          _ -> []

workoutSquareB4 :: (Num a, Ord a) => Rule (Focus (PEX a))
workoutSquareB4 = lift2focus $ rPEX $ buggyRule "square of sum is sum of squares minus factor" f 
   where 
      f p = case p of 
          (a :*: b :+: c):^:2 -> [ a * b^2 - c^2]
          (c :+: (a :*: b)):^:2 -> [ c^2 - a * b^2]
          _ -> []

workoutSquareB5 :: (Num a, Ord a) => Rule (Focus (PEX a))
workoutSquareB5 = lift2focus $ rPEX $ buggyRule "(a+b)^2 -> a^2 + ab + b^2" f 
   where 
      f p = case p of 
          (a :+: b):^:2 -> [a^2 + a*b + b^2]
          _ -> []

squareB :: (Num a, Ord a) => Rule (Focus (PEX a))
squareB = lift2focus $ rPEX $ buggyRule "a^2 -> 2 * a" f 
   where 
      f p = case p of  
         N a :*: (b :*: (N c :*: d)) | a == c -> [N (2 * a) * b * d]
         N a :*: (b :*: N c) | a == c -> [N (2 * a) * b] 
         (N a :*: N b) :*: c| a == b -> [N (2 * a) * c]
         N a :*: N b | a == b -> [N (2 * a)]
         (N a :^: 2) :*: b -> [N (2 * a) * b]
         N a :^: 2 -> [N (2 * a)] 
         _ -> []

divideReverse :: (Fractional a, Ord a) => Rule (Focus (PEQ a))
divideReverse = 
   lift2focus $ rPEQ $ buggyRule "reversely divide by a in a*B=c" f
   where 
      f p = case p of 
         N a :*: b :=: N c | c /= 0 -> [b :=: N (a/c)]
         _ -> []

divideSubtract :: (Num a, Ord a) => Rule (Focus (PEQ a))
divideSubtract = 
   lift2focus $ rPEQ $ buggyRule "subtract a in a*B=c" f
   where 
      f p = case p of 
         N a :*: b :=: N c -> [b :=: N (c - a)]
         _ -> []

divideForget :: (Num a, Eq a) => Rule (Focus (PEQ a))
divideForget = 
   lift2focus $ rPEQ $ buggyRule "forget a in a*B=c" f
   where 
      f p = case p of 
         N a :*: b :=: N c | a /= 1 -> [b :=: N c]
         _ -> []

nillProductB :: (Num a, Ord a) => Rule (Focus (PQS a))
nillProductB = 
   lift2focus $ rPQS $ buggyRule "A*B=C => A=C or B=C" f
   where 
      f p = case p of 
         OrList [a :*: b :=: c] | c /= 0 -> [OrList [a :=: c, b :=: c]]
         _ -> []

approximateRoots :: Int -> Rule (Focus (PQS ComRoot))
approximateRoots n = lift2focus $ rPQS $ buggyRule "approximate roots" f
   where 
      f p = if fmap g p == p
            then []
            else [fmap g p]
      g a 
         | isRat a = a
         | imp a /= 0 = a
         | otherwise = re $ ER.constant $ rounder n $ ER.eval $ rep a
                
roundRootsUp :: Int -> Rule (Focus(PQS ComRoot))
roundRootsUp n = lift2focus $ rPQS $ buggyRule "approximate roots" f
   where f p = if r p == p then [] else [r p]
         r = mapToOr (mapEq (fmap g)) 
         g x = re $ ER.constant $ roundUp n $ ER.eval $ rep x

roundRootsDown :: Int -> Rule (Focus(PQS ComRoot))
roundRootsDown n = lift2focus $ rPQS $ buggyRule "approximate roots" f
   where f p = if r p == p then [] else [r p]
         r = mapToOr (mapEq (fmap g)) 
         g x = re $ ER.constant $ roundDown n $ ER.eval $ rep x

roundSol :: Int ->  Rule (Focus(PQS ComRoot))
roundSol n = lift2focus $ rPQS $ buggyRule "approximate a number" f
   where 
      f p = case or2list p of 
               [ Var "x" :=: N _
                  , Var "x" :=: N _] -> traverse g p
               [ Var "x" :=: N _ ] -> traverse g p
               _ -> []    
      g a = [u a] ++ [d a] 
      u x = re $ ER.constant $ roundUp n $ ER.eval $ rep x
      d x = re $ ER.constant $ roundDown n $ ER.eval $ rep x
      
squareAroot :: Rule (Focus (PEX ComRoot))
squareAroot =
   lift2focus $ rPEX $ buggyRule "sqrt a -> a" f
   where 
      f p = case p of 
         N a | isRat a -> []
             | otherwise -> [N (a^2)]
         _ -> []    

linSquareForgetRoot :: Rule (Focus (PEQ ComRoot))
linSquareForgetRoot = 
   lift2focus $ rPQS $ buggyRule "A^2 = b -> A = b, A = - b" f 
   where 
      f p = case p of 
         OrList [N a :*: (x:^:2) :=: N b] -> 
            [OrList [N a * x :=: N (-b), N a * x :=: N b]]
         OrList [x:^:2 :=: N b] -> [OrList [x :=: N (-b), x :=: N b]]
         _ -> []

linSquareForgetRootConst :: Rule (Focus (PEQ ComRoot))
linSquareForgetRootConst = 
   lift2focus $ rPQS $ buggyRule "aA^2 = b -> aA = sqrt b, aA = - sqrt b" f 
   where 
      f p = let r = sqrtSim in case p of 
         OrList [N a :*: (x:^:2) :=: N b] -> 
            [OrList [N a * x :=: N (-r b), N a * x :=: N (r b)]]
         _ -> []

abcForgetRoot :: Rule (Focus (PQS ComRoot))
abcForgetRoot = lift2focus $ rPQS $ buggyRule "forget root in the abcFormula" f 
   where 
      f p = case mapOr toZero p of 
         [q] | degree q == 2 -> 
            [OrList [ var "x" :=: N ((-b - d)/(2*a))
                    , var "x" :=: N ((-b + d)/(2*a))]]
            where a = getCoeff "x" 2 q
                  b = getCoeff "x" 1 q
                  c = getCoeff "x" 0 q
                  d = b^2-4*a*c
         _ -> []

abcDivideBy_a :: Rule (Focus (PQS ComRoot))
abcDivideBy_a = lift2focus $ rPQS $ buggyRule "divide by a in the abcFormula" f 
   where 
      f p = case mapOr toZero p of 
         [q] | degree q == 2 -> 
            [OrList [ var "x" :=: N ((-b - sqrtSim d)/a)
                    , var "x" :=: N ((-b + sqrtSim d)/a)]]
            where a = getCoeff "x" 2 q
                  b = getCoeff "x" 1 q
                  c = getCoeff "x" 0 q
                  d = b^2-4*a*c
         _ -> []

abcForgetSquareB :: Rule (Focus (PQS ComRoot))
abcForgetSquareB  = lift2focus $ rPQS $ buggyRule "forget to square b in the abcFormula" f 
   where 
      f p = case mapOr toZero p of 
         [q] | degree q == 2-> 
            [OrList [ var "x" :=: N ((-b - sqrtSim d)/(2*a))
                    , var "x" :=: N ((-b + sqrtSim d)/(2*a))]]
            where a = getCoeff "x" 2 q
                  b = getCoeff "x" 1 q
                  c = getCoeff "x" 0 q
                  d = b-4*a*c
         _ -> []

abcNegativeSquareB :: Rule (Focus (PQS ComRoot))
abcNegativeSquareB = lift2focus $ rPQS $ buggyRule "use a negative square of b in the abcFormula" f 
   where 
      f p = case mapOr toZero p of 
         [q] | degree q == 2-> 
            [OrList [ var "x" :=: N ((-b - sqrtSim d)/(2*a))
                    , var "x" :=: N ((-b + sqrtSim d)/(2*a))]]
            where a = getCoeff "x" 2 q
                  b = getCoeff "x" 1 q
                  c = getCoeff "x" 0 q
                  d = -b^2-4*a*c
         _ -> []


discWithPlus :: (Num a, Ord a) => Rule (Focus (PEQ a))
discWithPlus = lift2focus $ rPEQ $ buggyRule "calculate discriminant with plus" f 
   where 
      f p = if degree q == 2 
         then [var "D" :=: N (b^2 + 4*a*c)]
         else []
         where q = toZero p
               a = getCoeff "x" 2 q
               b = getCoeff "x" 1 q
               c = getCoeff "x" 0 q

discForgetSquareB :: (Num a, Ord a) => Rule (Focus (PEQ a))
discForgetSquareB = lift2focus $ rPEQ $ buggyRule 
   "calculate discriminant forget to square b" f 
   where 
      f p = if degree q == 2 
         then [var "D" :=: N (b - 4*a*c)]
         else [] 
         where q = toZero p
               a = getCoeff "x" 2 q
               b = getCoeff "x" 1 q
               c = getCoeff "x" 0 q


discNegativeSquareB :: (Num a, Ord a) => Rule (Focus (PEQ a))
discNegativeSquareB = lift2focus $ rPEQ $ buggyRule 
   "calculate discriminant with a negative square b" f 
   where 
      f p = if degree q == 2 
         then [var "D" :=: N (-b^2 - 4*a*c)]
         else [] 
         where q = toZero p
               a = getCoeff "x" 2 q
               b = getCoeff "x" 1 q
               c = getCoeff "x" 0 q