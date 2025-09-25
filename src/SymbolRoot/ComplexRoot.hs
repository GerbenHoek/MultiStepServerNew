module SymbolRoot.ComplexRoot 
   ( ComRoot
   , highRoot
   , sqRoot
   , constant
   , simplify
   , eval
   , sqrtSim
   , isInt
   , isRat
   , getInt
   , getRat
   , im
   , imp 
   , re
   , rep) where 
import SymbolRoot.ExprRoot (ExprRoot)
import qualified SymbolRoot.ExprRoot as ER
import Utils.ComplexNum
import Data.Maybe (isJust)

type ComRoot = Complex ExprRoot

highRoot :: Integer -> ComRoot -> ComRoot
highRoot k r  
   | imp r /= 0 = error "only defined for real numbers"
   | signum a >= 0 = re $ ER.highRoot k a
   | signum a <  0 && even k = im $ ER.highRoot k $ abs a
   | otherwise = - (re $ ER.highRoot k $ abs a)  
   where a = rep r

sqRoot :: ComRoot -> ComRoot
sqRoot = highRoot 2 

constant :: Rational -> ComRoot
constant = re . ER.constant

simplify :: ComRoot -> ComRoot
simplify = fmap ER.simplify

eval :: ComRoot -> Complex Double
eval = fmap ER.eval

sqrtSim :: ComRoot -> ComRoot
sqrtSim = simplify . sqRoot

getInt :: ComRoot -> Maybe Integer
getInt r = if imp r == 0 
           then ER.getInt (rep r)
           else Nothing 

getRat :: ComRoot -> Maybe Rational
getRat r = if imp r == 0 
           then ER.getRat (rep r)
           else Nothing 

isInt :: ComRoot -> Bool
isInt = isJust . getInt

isRat :: ComRoot -> Bool
isRat = isJust . getRat