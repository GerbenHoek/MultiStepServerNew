module Polynomial.MultiPoly 
   ( MultiPoly
   , var 
   , getVars
   , getTerms
   , constant
   , numberOfTerms
   , leadCoeff
   , degree
   , monic
   , factorPoly
   , distributePoly
   , factorisations
   , divideByVar
   , recipConstant) where 
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (intercalate, maximumBy)

type Power x = Map x Int

type Term x a = (Power x, a)

showTerm :: (Ord x, Num a, Eq a, Show a, Show x) => Term x a -> String
showTerm (n, a) = if null n then show a else 
   (if a == 1 then "" else show a ++ " * ") ++ 
   (intercalate " * " $ (map sh $ Map.toList n))
      where 
         sh (m, k) = "(" ++ show m ++  
            (if k == 1 then ")" else ")^" ++ show k)

powerOfVar :: Ord x => x -> Term x a -> Int
powerOfVar n (ks,_) = Map.findWithDefault 0 n ks

hasVar :: Ord x => x -> Term x a -> Bool
hasVar n = (/=0) . powerOfVar n

data MultiPoly x a = MP {getP :: Map (Power x) a} 
   deriving (Eq, Ord)

instance (Num a, Eq a, Show a, Ord x, Show x) => Show (MultiPoly x a) where 
   show = intercalate " + " . map showTerm . getTerms 

getTerms :: Ord x => MultiPoly x a -> [Term x a]
getTerms = Map.toDescList . getP 

getVars :: Ord x => MultiPoly x a -> [x]
getVars = 
   Set.toList . 
   Set.unions . 
   map Map.keysSet . 
   Map.keys . 
   getP

numberOfTerms :: MultiPoly x a -> Int
numberOfTerms = Map.size . getP

cleanPoly
  :: (Num a, Ord x, Eq a) => MultiPoly x a -> MultiPoly x a
cleanPoly p = let p' = Map.filter (/=0) $ getP p in 
               if null p'
               then MP $ Map.singleton Map.empty 0
               else MP p' 

instance Functor (MultiPoly x) where 
   fmap g = MP . Map.map g . getP

toPoly :: (Eq a, Num a, Ord x) => [Term x a] -> MultiPoly x a
toPoly = cleanPoly . MP . Map.fromListWith (+)

constantTerm :: (Eq a, Num a, Ord x) => MultiPoly x a -> Term x a
constantTerm p = case Map.lookup Map.empty (getP $ cleanPoly p) of 
   Nothing -> (Map.empty, 0)
   Just a  -> (Map.empty, a)

recipConstant :: 
   (Eq a, Fractional a, Ord x) => MultiPoly x a -> MultiPoly x a
recipConstant p = let (e, a) = constantTerm p in 
   toPoly [(e, 1/a)]

leadTerm :: Ord x => MultiPoly x a -> Term x a
leadTerm = Map.findMax . getP 
  
leadForVar :: (Eq a, Num a, Ord x) => x -> MultiPoly x a -> Term x a
leadForVar n p = 
   case filter (hasVar n) (getTerms p) of 
   [] -> constantTerm p 
   ts -> maximumBy comp ts 
   where comp a b = compare (powerOfVar n a) (powerOfVar n b) 

leadCoeff :: Ord x => MultiPoly x a -> a
leadCoeff = snd . leadTerm

degree :: Ord x => MultiPoly x a -> Power x
degree = fst . leadTerm

degreeForVar :: (Eq a, Num a, Ord x) => x -> MultiPoly x a -> Int
degreeForVar n = powerOfVar n . leadForVar n 

monic :: (Fractional a, Ord x) => MultiPoly x a  -> MultiPoly x a
monic p = fmap (*(1/an)) p
   where an = leadCoeff p

constant :: (Eq a, Num a, Ord x) => a -> MultiPoly x a
constant a = toPoly [(Map.empty, a)]

var :: (Eq a, Num a, Ord x) => x -> MultiPoly x a
var n = toPoly [(Map.singleton n 1, 1)]

mul :: (Num a, Ord x) => Term x a -> Term x a -> Term x a 
mul (ns, a) (ms, b) = (ks, a * b)
   where ks = Map.unionWith (+) ns ms

sign :: (Eq a, Ord x, Num a) => MultiPoly x a -> MultiPoly x a
sign = constant . signum . leadCoeff 

instance (Eq a, Ord x, Num a) => Num (MultiPoly x a) where
   p + q = cleanPoly $ MP $ Map.unionWith (+) (getP p) (getP q)
   p * q = cleanPoly $ MP $ Map.fromListWith (+) $ 
                            mul <$> (getTerms p) <*> (getTerms q) 
   negate = fmap negate 
   abs p  = (sign p) * p
   signum = sign 
   fromInteger = constant . fromInteger

commonVars :: Ord x => MultiPoly x a -> Map x Int
commonVars = foldr1 (Map.intersectionWith min) . Map.keys . getP

divideByVar :: (Eq a, Num a, Ord x) => x -> MultiPoly x a -> MultiPoly x a 
divideByVar x p = if degreeForVar x p == 0 
                  then p
                  else cleanPoly $ MP $
                       Map.mapKeysWith (+) (Map.filter (/=0)) $ 
                       Map.mapKeys (Map.mapWithKey g) $ getP p
   where g y n = if y == x then n - 1 else n 

factorisations :: 
   (Eq a, Num a, Ord x) => 
      MultiPoly x a -> [(MultiPoly x a, MultiPoly x a)]
factorisations p = 
   let divide = flip (divideByVar) p in 
   [ (var x, divide x) 
   | (x, _) <- Map.toList $ commonVars p
   , divide x /= 1]

factorPoly ::
   (Num a, Ord x, Eq a) =>
     x -> MultiPoly x a -> [(Maybe (x, Int), MultiPoly x a)]
factorPoly x p = 
   Map.toList $
   Map.fromListWith (+) $ 
   map (factorTerm x) (getTerms p) 
   where 
      factorTerm y (ns, a) = case Map.lookup y ns of 
         Nothing -> (Nothing, toPoly [(ns, a)])
         Just n  -> (Just (y, n), toPoly [(Map.delete y ns, a)])

distributePoly
  :: (Num a, Ord x, Eq a) =>
     [(Maybe (x, Int), MultiPoly x a)] -> MultiPoly x a
distributePoly ps = foldr (+) 0 [trm t * p | (t, p) <- ps]
   where trm s = case s of 
                    Nothing     -> 1 
                    Just (x, n) -> var x ^ n