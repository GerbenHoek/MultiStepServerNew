module SymbolRoot.SymbolRoot 
   ( SymbolRoot
   , constant
   , highRoot
   , getValues
   , getLeadCoef
   , numberOfTerms
   , degree) where 
import SymbolRoot.FactorInteger hiding (eval)
import qualified SymbolRoot.FactorInteger as FI (eval)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ratio
import Polynomial.LoopDiv
import Polynomial.MultiPoly (MultiPoly)
import qualified Polynomial.MultiPoly as MP
import Polynomial.SinglePoly (SinglePoly)
import qualified Polynomial.SinglePoly as SP

newtype SymbolRoot = SR (Map (Integer, FactorInteger) Rational)
   deriving (Eq, Ord) 

constant :: Rational -> SymbolRoot 
constant a = SR $ Map.singleton (1, 1) a

getRoots :: SymbolRoot -> Map (Integer, FactorInteger) Rational
getRoots (SR n) = n

getValues :: SymbolRoot -> [((Integer, Integer), Rational)]
getValues r = [((n, FI.eval k), a)| ((n, k), a) <- Map.toList $ getRoots r]

getLeadCoef :: SymbolRoot -> Rational 
getLeadCoef r = let (_,a) = Map.findMax $ getRoots r in a

numberOfTerms :: SymbolRoot -> Int
numberOfTerms = Map.size . getRoots

degree :: SymbolRoot -> Integer
degree = fst . fst . Map.findMax . getRoots

eval :: SymbolRoot -> Double 
eval r = sum [f a| a <- Map.toList $ getRoots r] 
   where 
      f ((p, k), n) = 
         fromRational n * 
         (fromIntegral $ abs $ FI.eval k) ** 
         (1 / fromIntegral p)

type Root = ((Integer, FactorInteger), Rational)

showRoot :: Root -> String 
showRoot ((p, k), a) 
   | a == 1 && k /= 1 && p /= 1 = show p ++ "^√(" ++ show k ++ ")" 
   | k == 1 = show a 
   | p == 1 = show $ p * FI.eval k
   | otherwise = show a ++ " * " ++ show p ++ "^√(" ++ show k ++ ")" 

instance Show SymbolRoot where 
   show = intercalate " + " . map showRoot . Map.toList . getRoots  

sign :: SymbolRoot -> SymbolRoot 
sign r
   | eval r > 0  = constant 1
   | eval r < 0  = constant (-1)
   | otherwise   = constant 0

(.*.) :: Root -> Root -> Root 
((p, n), a) .*. ((q, m), b) = ((p', k'), a * b * fromIntegral c)
   where (k', p') = commonPower (p*q) k 
         (c, k)   = getPowers (p*q) (abs $ n^q * m^p)

zeroCoef :: SymbolRoot -> SymbolRoot 
zeroCoef r = 
   let c = Map.filterWithKey 
          (\(_,n) s -> s /= 0 && n /= 0) (getRoots r) in 
   if Map.null c then SR $ Map.singleton (1,1) 0 else SR c 

instance Num SymbolRoot where 
   a + b = zeroCoef $ SR $ Map.unionWith (+) (getRoots a) (getRoots b)
   a * b = zeroCoef $ SR $ Map.fromListWith (+) $
       (.*.) <$> (Map.toList $ getRoots a) <*> (Map.toList $ getRoots b)
   signum = sign 
   fromInteger a = SR $ Map.singleton (1, 1) (fromInteger a)
   negate a = SR $ Map.map (* (-1)) (getRoots a)
   abs a = (sign a) * a

highRoot' :: Integer -> Integer -> SymbolRoot
highRoot' p a  = 1 * (SR $ Map.singleton (p, toFactorInteger a) 1)

rootPoly 
   :: (Num a, Eq a) 
      => (Integer, FactorInteger) 
         -> SinglePoly (Integer, FactorInteger) a
rootPoly (p, a) = SP.var (p, a) ^ p - SP.constant (fromIntegral $ FI.eval a)

divide
  :: MultiPoly (Integer, FactorInteger) Rational
     -> MultiPoly (Integer, FactorInteger) Rational 
        -> MultiPoly (Integer, FactorInteger) Rational
divide a b = 
   if null $ MP.getVars b 
   then a * (MP.recipConstant b)
   else a * (SP.distributePoly $ inverseOver divide poly (rootPoly var))
      where var = head $ MP.getVars b 
            poly = SP.factorPoly var b 

toMulti :: SymbolRoot -> MultiPoly (Integer, FactorInteger) Rational
toMulti = foldr1 (+) . map f . Map.toList . getRoots 
   where f (r, a) = if r == (1,1) 
                    then MP.constant a 
                    else MP.constant a * MP.var r  

fromMulti :: MultiPoly (Integer, FactorInteger) Rational -> SymbolRoot
fromMulti = foldr1 (+) . map f . MP.getTerms
   where 
      f (p, a) = 
         if null p 
         then constant a
         else constant a * (foldr1 (*) $ map g $ Map.toList p)
              where g (r, k) = (SR $ Map.singleton r 1) ^ k


evalFraction :: SymbolRoot -> SymbolRoot -> SymbolRoot
evalFraction a b = fromMulti $ divide (toMulti a) (toMulti b)

instance Fractional SymbolRoot where 
   fromRational = constant
   a / b = evalFraction a b  

highRoot :: Integer -> Rational -> SymbolRoot
highRoot k a     
   | k == 0 = 1
   | k == 1 = constant a 
   | a >= 0 = n / d
   | otherwise = error "only defined for positive values"  
   where n = highRoot' k $ numerator a
         d = highRoot' k $ denominator a