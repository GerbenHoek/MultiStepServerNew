module SymbolRoot.FactorInteger 
   ( FactorInteger
   , toFactorInteger
   , eval
   , getPowers
   , commonPower) where 
import Data.Map (Map)
import qualified Data.Map as Map

type Term = (Integer, Map Integer Integer)
newtype FactorInteger = FI [Term] 

getTerms :: FactorInteger -> [Term]
getTerms (FI a) = a

factor :: Integer -> [Integer]
factor 0 = [0]
factor (-1) = [1]
factor x = let k = abs x in  
   if k == 1 then []
   else let (p, k') = getLeast k in p:factor k' 
   where 
      getLeast m = 
         case [(n, m `div` n) | n <- [2..m], rem m n == 0]
         of []  -> (1, m)
            a:_ -> a

toFactorInteger :: Integer -> FactorInteger
toFactorInteger k = FI $ [(signum k, Map.fromListWith (+) $ zip (factor k) (repeat 1))]

eval :: FactorInteger -> Integer
eval n = sum [s * product [a ^ b | (a, b) <- Map.toList t] | (s, t) <- getTerms n]

instance Eq FactorInteger where 
   a == b = (eval a) == (eval b)

instance Ord FactorInteger where 
   a `compare` b = (eval a) `compare` (eval b)

instance Show FactorInteger where 
   show = show . eval  

(.*.) :: Term -> Term -> Term
(s, n) .*. (t, m) = (s * t, Map.unionWith (+) n m)

instance Num FactorInteger where 
   a + b = FI $ (getTerms a) ++ (getTerms b) 
   a * b = FI $ (.*.) <$> (getTerms a) <*> (getTerms b)
   signum a = case a of FI [n] -> FI [(fst n, Map.empty)] 
                        _      -> toFactorInteger $ signum $ eval a 
   abs a = case a of FI [(s, n)] -> FI [(abs s, n)] 
                     _      -> toFactorInteger $ abs $ eval a 
   negate a = case a of FI [(s, n)] -> FI [(negate s, n)] 
                        _      -> toFactorInteger $ negate $ eval a                        
   fromInteger = toFactorInteger . fromInteger

clean :: FactorInteger -> FactorInteger
clean n = FI $ map f (getTerms n)
   where 
      f (s, a) = (s, Map.filter (/=0) a) 

getPowers :: Integer -> FactorInteger -> (Integer, FactorInteger)
getPowers k a = 
   case a of 
      FI [(s, n)] -> ( eval $ FI [(1, Map.filter (/=0) $ Map.map (`div` k) n)]
                     , clean $ FI [(s, Map.map (`rem` k) n)])             
      _      -> getPowers k $ toFactorInteger $ eval a

commonPower :: Integer -> FactorInteger -> (FactorInteger, Integer)
commonPower k a = case a of 
   FI [(s, n)] -> let p = gcdl $ k:(Map.elems n) 
             in (clean $ FI [(s, Map.map (`div` p) n)], k `div` p)      
   _      -> commonPower k $ toFactorInteger $ eval a
   where gcdl as = foldr1 gcd as