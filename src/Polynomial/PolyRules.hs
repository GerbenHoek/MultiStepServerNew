module Polynomial.PolyRules where 
import Polynomial.ExprPoly hiding (rebracket, simplify, constant)
import qualified Polynomial.ExprPoly as EP (rebracket, simplify)
import FSMMaker.Rules 
import Regulation.Focus 
import Polynomial.PolyLift hiding (rhs)
import Data.List (permutations)
import Data.Maybe (maybeToList)
import SymbolRoot.ComplexRoot hiding (eval)

showStep :: Rule a 
showStep = rule "show step" (\a->[a])

eval :: (Num a, Ord a) => Rule (Focus (PQS a))
eval = lift2focus $ rPQS $ rule "evaluate equation" f 
   where 
      f p = let q = mapToOr (mapEq keepFactors) p in
         if q == p then [] else [q]

evalMinor :: (Num a, Ord a) => Rule (Focus (PQS a))
evalMinor = makeMinor eval 

derive2zero :: (Num a, Ord a) => Rule (Focus (PEQ a))
derive2zero = lift2focus $ rPEQ $ rule "derive to zero" f 
   where 
      f p = case p of 
         _ :=: 0 -> []
         a :=: b -> [a .-. b :=: 0]

nillproduct :: (Num a, Ord a) => Rule (Focus (PQS a))
nillproduct = lift2focus $ rPQS $ rule "nillProduct" f 
   where 
      f p = case p of 
         OrList [a :*: b :=: 0] -> [OrList [a :=: 0, b :=: 0]]
         _ -> []

removeDegen :: (Num a, Ord a) => Rule (Focus (PQS a))
removeDegen = lift2focus $ rPQS $ rule "remove degenerate equation" f 
   where 
      f p = case p of 
         OrList [q] |degree(toZero q) == 0 -> [OrList []]
         _ -> []

hasZeroRoot :: Rule (Focus (PQS ComRoot))
hasZeroRoot = lift2focus $ rPQS $ rule "has a solution x = 0" f 
   where f p = let isZR (Var "x" :=: 0) = True
                   isZR _               = False
               in case solveDegLT2 p of 
                     [OrList q] | any isZR q -> [p]
                     _ -> []

factor :: (Num a, Ord a) => Rule (Focus (PEX a))
factor = lift2focus $ rPEX $ rule "factor" f 
   where 
      f p = case p of 
         a :*: b :+: (a' :*: c) 
            | a == a' -> [a * (b + c)]
         _ -> []

permutateFactors :: (Num a, Ord a) => Rule (Focus (PEX a))
permutateFactors = 
   lift2focus $ 
   rPEX $ 
   minorRule "permutate toplevel factors in toplevel terms" f 
   where
      f = foldr1 lift . map g . terms2list 
      g = map (foldr1 (*)) . permutations . factors2list 
      lift x y = (+) <$> x <*> y

factorDirect :: (Num a, Ord a) => Rule (Focus (PQS a))
factorDirect = lift2focus $ rPQS $ rule "AB=AC -> A = 0 or B = C" f
   where 
      f p = case p of 
         OrList [a :*: b :=: a' :*: c] | a == a' -> [OrList [b:=:c, a:=:0]]
         _ -> []

nillProduct :: (Num a, Ord a) => Rule (Focus (PQS a))
nillProduct = lift2focus $ rPQS $ rule "nillProduct" f 
   where 
      f p = case p of
         OrList [a :*: b :=: 0] -> [OrList [a :=: 0, b :=:0]]
         _-> []  

distribute' :: (Num a, Ord a) => Rule (ExprPoly String a)
distribute' = rule "distribute" f 
   where 
      f p = case p of
         a :*: (b :*: c) 
            | isSum a || isSum b -> [(g a b) * c]
         a :*: b 
            | isSum a || isSum b -> [g a b]
         _ -> []
      isSum (_ :+: _) = True
      isSum _         = False
      g x y = foldr1 (+) [ x' * y' 
                         | x' <- terms2list x
                         , y' <- terms2list y]

distribute :: (Num a, Ord a) => Rule (Focus (PEX a))
distribute = lift2focus $ rPEX $ distribute'

distributeConst :: (Num a, Ord a) => Rule (Focus (PEX a))
distributeConst = lift2focus $ rPEX $ rule "distributeConst" f
   where 
      f p = case p of 
         N a :*: b -> applyRule distribute' p
         _ -> []

workoutSquare :: (Num a, Ord a) => Rule (Focus (PEX a))
workoutSquare = lift2focus $ rPEX $ rule "write a square as a product" f 
   where 
      f p = case p of 
        (a:+:b):^:2 :+: c -> [(a+b)*(a+b) + c]
        (a:+:b):^:2 -> [(a+b)*(a+b)]
        _ -> []

rebracket :: (Num a, Ord a) => Rule (Focus (PQS a))
rebracket = lift2focus $ rPQS $ minorRule "rebracket" f 
   where 
      f p = let q = mapToOr (mapEq EP.rebracket) p in
         if q == p then [] else [q]

divideByFirst :: (Fractional a, Ord a) => Rule (Focus (PEQ a))
divideByFirst = 
   lift2focus $ rPEQ $ rule "divide equation by the first factor in _ = c" f
   where 
      f p = case p of 
         N a :*: b :=: N c | a /= 1 && a /= 0-> [b :=: N (c/a)]
         _ -> []

transportConstant :: (Ord a, Num a) => Rule (Focus (PEQ a))
transportConstant = 
   lift2focus $ rPEQ $ rule "move constant term over equality sign in _ = c" f
   where 
      f p = case p of 
         a :+: N b :+: c :=: N d  -> [a :+: c :=: N (d - b)]
         a :+: N b :=: N c -> [a :=: N (c - b)]
         N a :+: b :=: N c -> [b :=: N (c - a)]
         _ -> []

linSquare :: Rule (Focus (PQS ComRoot))
linSquare = 
   lift2focus $ rPQS $ rule "A^2 = b -> A = sqrt b, A = - sqrt b" f 
   where 
      f p = let r k = if isInt $ sqRoot k 
                      then sqrtSim k 
                      else sqRoot k
            in case p of 
         OrList [N a :*: (x:^:2) :=: N b] -> 
            [OrList [N (r a) * x :=: N (-r b), N (r a) * x :=: N (r b)]]
         OrList [x:^:2 :=: N b] -> [OrList [x :=: N (-r b), x :=: N (r b)]]
         _ -> []

flipEq ::  Rule (Focus (PEQ a))
flipEq = 
  lift2focus $ rPEQ $ minorRule "flip sides of an equation" f 
  where 
     f p = case p of 
        l :=: r -> [r :=: l]

factorThreeTerm :: Rule (Focus (PEX ComRoot))
factorThreeTerm = lift2focus $ rPEX $ rule "factor three term" f 
   where 
      f p = if termForm p && 
               a == 1 &&
               isInt x1 &&
               isInt x2 
            then [(var "x" - N x1) * (var "x" - N x2)]
            else []
            where 
               x1 = (-b - sqrtSim (b^2-4*c))/2 
               x2 = (-b + sqrtSim (b^2-4*c))/2 
               a = getCoeff "x" 2 p
               b = getCoeff "x" 1 p
               c = getCoeff "x" 0 p

factorTwoTerm :: Rule (Focus(PEX ComRoot))
factorTwoTerm = lift2focus $ rPEX $ rule "factor three term" f 
   where 
      f p = if termForm p && c == 0 && a /= 0 && b /= 0
            then [ var "x" * (N a * var "x" + N b)
                 , -var "x" * (N (-a) * var "x" + N (-b)) ]
            else []
         where a = getCoeff "x" 2 p
               b = getCoeff "x" 1 p
               c = getCoeff "x" 0 p

factorLead :: (Fractional a, Ord a) => Rule (Focus (PEQ a))
factorLead = lift2focus $ rPEQ $ rule "factor out lead coefficient" f
   where 
      f p = case p of 
         q :=: 0 | termForm q && 
                   leadCoeff q /= 1 -> 
            [ N (leadCoeff q) * monic q :=: 0
            , N (-leadCoeff q) * (mapTerms ((-1)*) (monic q)) :=: 0]
         _ -> [] 

factorDivisor :: Rule (Focus (PEQ ComRoot))
factorDivisor  = lift2focus $ rPEQ $ rule "factor out a divisor" f
   where 
      f p = case p of 
         q :=: 0 -> [N n * mapTerms (N (1/n)*) q :=: 0| n <- divs]
            where divs = [constant $ fromIntegral k
                         | d <- gcd', d/=1
                         , k <- [2..d], d `rem` k == 0]
                  gcd' = maybeToList $ (foldr1 gcd) <$> traverse f [0..2] 
                  f y = getInt $ getCoeff "x" y q 
         _ -> []

completeSquare :: (Fractional a, Ord a) => Rule (Focus (PEX a))
completeSquare = lift2focus $ rPEX $ rule "complete the square" f 
   where 
      f p = if termForm p && a == 1 
            then [(var "x" + N(b/2)):^:2 + N (c - (b/2)^2)]
            else []
         where a = getCoeff "x" 2 p
               b = getCoeff "x" 1 p
               c = getCoeff "x" 0 p

discriminant :: (Num a, Ord a) => Rule (Focus (PEQ a))
discriminant = lift2focus $ rPEQ $ rule "calculate discriminant" f
   where 
      f p = case p of 
         q :=: 0 | termForm q && degree q == 2 
            -> [Var "D" :=: N b.^.2 - 4 * N a * N c]
            where a = getCoeff "x" 2 q
                  b = getCoeff "x" 1 q
                  c = getCoeff "x" 0 q
         _ -> []

abcFromDisc :: Rule (Focus (PQS ComRoot))
abcFromDisc = lift2focus $ rPQS $ rule "abc-formula from discriminant" f
   where 
      f p = case p of
         OrList [Var "D" :=: (N b :^: 2) :+: (N (-1) :*: (N 4 :*: (N a :*: N c)))]
            -> [OrList [ var "x" :=: N ((-b - sqRoot d)/(2*a)) 
                       , var "x" :=: N ((-b + sqRoot d)/(2*a)) ]]
            where d = getCoeff "D" 0 $ EP.simplify $ rhs $ head $ or2list p
         _ -> []

abcFormula :: Rule (Focus (PQS ComRoot))
abcFormula = lift2focus $ rPQS $ rule "abc-formula" f 
   where 
      f p = case p of 
         OrList [q :=: 0] | termForm q -> [OrList $ solveDeg2 (q :=: 0)] 
         _ -> []

helpSort :: (Ord a, Num a) => PolyEq String a -> [PolyEq String a]
helpSort p = case p of 
        N _ :*: Var "x" :=: N _ -> []
        Var "x" :=: N _ -> []
        _ | degree (toZero p) == 1 && a == 1 -> [var "x" :=: N (-b)]
          | degree (toZero p) == 1 -> [N a * var "x" :=: N (-b)]
          | otherwise -> []
           where 
              a = getCoeff "x" 1 (toZero p)
              b = getCoeff "x" 0 (toZero p)

sortLin :: (Ord a, Num a) => Rule (Focus (PEQ a))
sortLin = lift2focus $ rPEQ $ rule "place variable term on the lhs" helpSort

abcAlways :: Rule (Focus (PQS ComRoot))
abcAlways = lift2focus $ rPQS $ rule "always find roots" f 
   where 
      f p = case p of 
         OrList [q] -> [OrList $ solveDeg2 q] 
         _ -> []

allLin :: (Num a, Ord a) => Rule (Focus (PQS a))
allLin = lift2focus $ rPQS $ minorRule "test if all equations are linear" f
   where 
      f p 
         | all tst $ or2list p = [p]
         | otherwise = [] 
      tst = (<=1) . exprDegree . toZero 

allDeg2 :: (Num a, Ord a) => Rule (Focus (PEQ a))
allDeg2 = lift2focus $ rPQS $ minorRule "test if all equations are snd degree" f
   where 
      f p 
         | all tst $ or2list p = [p]
         | otherwise = [] 
      tst = (==2) . degree . toZero 

sortAllLin :: (Num a, Ord a) => Rule (Focus (PQS a))
sortAllLin = lift2focus $ rPQS $ rule "sort all linear equations" f
   where f = (:[]) . mapToOr g 
         g p = case helpSort p of 
                  []   -> p
                  [q]  -> q
                  _    -> p

simplifyAllRoots :: Rule (Focus (PEQ ComRoot)) 
simplifyAllRoots = lift2focus $ rPQS $ rule "simplify all roots" f
   where f p = if g p == p then [] else [g p]
         g = mapToOr (mapEq (fmap simplify))  

distributePath :: Rule a
distributePath = path "distribute"

nillProductPath :: Rule a
nillProductPath = path "A*B = 0 => A = 0 or B = 0"

linSquarePath :: Rule a
linSquarePath = path "A^2 = b => A = sqrt b or A = -sqrt b"

factorTwoTermPath :: Rule a
factorTwoTermPath = path "ax^2 + bx = 0 => x(ax+b) = 0"

factorThreeTermPath :: Rule a
factorThreeTermPath = path "x^2 - (a+b)x + ab = 0 => (x-a)(x-b) = 0"

completeSquarePath :: Rule a
completeSquarePath = path "complete square"

dividePath :: Rule a
dividePath = path $ ruleName divideByFirst

doubleFactorPath :: Rule a 
doubleFactorPath = path "A*B = A*C => A = 0 or B = C"

abcPath :: Rule a 
abcPath = path "abc-formula"

linPath :: Rule a
linPath = path "solve linear equations(s)"