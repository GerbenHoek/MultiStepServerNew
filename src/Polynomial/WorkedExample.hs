module Polynomial.WorkedExample where 
import FSMMaker.StrategyBas (allDerivations)
import Polynomial.PolyStrat (polyStrat)
import Polynomial.ExprPoly
import Regulation.Focus (top, embed)
import Polynomial.PolyLift (PQS(..))
import SymbolRoot.ComplexRoot (ComRoot)
import Communication.ParseExpr (parse2or)
import Utils.Utils (replace)
import Data.List (minimumBy)

normal :: (Num a, Ord a, Ord x) => OrList x a -> OrList x a
normal = nubOr . mapToOr (mapEq keepFactors)

nubSeq :: Eq a => [a] -> [a]
nubSeq [] = []
nubSeq [x] = [x]
nubSeq (x:y:xs) = if x == y then nubSeq (y:xs) else x:nubSeq (y:xs)

data Var = X | D 
   deriving (Eq, Ord)

instance Show Var where 
   show X = "x"
   show D = "D"

showPoly :: ExprPoly String ComRoot -> ExprPoly Var ComRoot
showPoly p = case p of 
   a :+: b -> showPoly a + showPoly b
   a :*: b -> showPoly a * showPoly b
   a :^: n -> showPoly a .^. n
   Var "x" -> Var X
   Var "D" -> Var D
   N a -> N a

showEq = OrList . mapOr f 
   where f (a:=:b) = showPoly a :=: showPoly b

--normalD
normalD p = 
   if any isDisc $ or2list p 
   then [p, normal p]
   else [normal p]

isDisc (Var D :=: _) = True 
isDisc _ = False

workedExample :: OrList String ComRoot -> [OrList Var ComRoot]
workedExample = let c x y = compare (length x) (length y) in
   reverse . minimumBy c .
   map (nubSeq . concatMap (normalD . showEq . pqs . top)) .
   allDerivations polyStrat . 
   embed . PQS

stringExample :: String -> [[String]]
stringExample p = case parse2or p of 
   Left msg -> error msg 
   Right q -> map makeString $ workedExample q
   where makeString r = case or2list r of 
            [s, t] -> [f s, f t, "?"]
            [s]    | isDisc s  -> ["?", "?", g s]
                   | otherwise -> [f s, "?", "?"]
            _      -> ["?","?", "?"]
         g      = replace "+" "%2B" . show . rhs 
         f      = replace "+" "%2B" . show 

tt :: OrList String ComRoot
tt = list2or --[(var "x" -3)*(var "x" +6) :=: (var "x" -2)*(3*var "x" +8)]
   --[(var "x" -1).^.2 :=: (var "x" -1)*(var "x" -2)]
    [var "x".^. 2 - 8*var "x" :=: 0]
--  [(var "x" - 2) * (var "x" + 2) :=: 10]
  --[var "x" .^. 2 + 7 * var "x" + 5 :=: 0]
  --[(-2) * var "x" ^2 + var "x"  -2 :=: 0]
   --[-11 + var "x".^.2 :=: 0]