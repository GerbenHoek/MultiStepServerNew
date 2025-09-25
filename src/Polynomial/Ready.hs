module Polynomial.Ready where 
import Polynomial.ExprPoly 
import Polynomial.PolyRelations (normalOr)
import Polynomial.PolyStrat (polyStrat)
import SymbolRoot.ComplexRoot hiding (simplify)
import qualified SymbolRoot.ComplexRoot as CR (simplify)
import FSMMaker.StrategyBas (shortestDerivation)
import Regulation.Focus (top, embed)
import Polynomial.PolyLift (PQS(..))
import Polynomial.WorkedExample (workedExample)
import Utils.Utils (setNub)

interOK :: [OrList String ComRoot] -> Bool 
interOK ps 
   | n <= 2 = s >= 2
   | n >  2 = s >  2 
   where  p = last ps 
          n = length $ workedExample p
          s = length $ setNub $ map moveEq $ ps
          moveEq q = mapToOr (mapEq simplify' . mapEq (a*)) q
             where simplify' = fmap CR.simplify . simplify 
                   a = case leadCoeff $ toZero $ head $ or2list q of 
                      0  -> 1
                      a' -> N (CR.simplify 1/a')

tt :: [OrList String ComRoot]
tt = map list2or [ [2*var "x" .^. 2 -4 * var "x" :=:0]
                 , [var "x" .^. 2 -2 * var "x" :=:0]
                 ]


ready :: [OrList String ComRoot] -> (String, Integer)
ready (p:ps) = case or2list $ simplify' p of 
   [Var "x" :=: N x, Var "x" :=: N y] -> redirect x y
   [Var "x" :=: N x] -> redirect x 0
   _ -> ("not ready", 2)
   where 
      simpleRoot r = simpleNum r && CR.simplify r /= r
      simpleNum r  = isRat r || isInt r  
      simplify'    = mapToOr (mapEq simplify)
      interMsg = "Voer nog een extra tussenstap in"
      rootMsg  = "Goed zo je bent er bijna, \
                 \je kunt de wortels nog wegwerken"
      numMsg   = "Goed zo je bent er bijna, \
                 \je kunt nog vereenvoudigen tot een getal"
      readyMsg = "Goed zo, de vergelijking is correct opgelost"
      redirect a b
         | not $ interOK (p:ps) = (interMsg, 5)
         | simpleRoot a 
         || simpleRoot b = (rootMsg, 0)
         | (simpleNum a 
         || simpleNum b) 
         && simplify' p /= p = (numMsg, 0)
         | otherwise  = (readyMsg, 4)