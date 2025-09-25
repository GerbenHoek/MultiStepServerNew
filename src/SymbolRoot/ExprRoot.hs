{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module SymbolRoot.ExprRoot 
   ( ExprRoot
   , constant
   , highRoot
   , sqRoot
   , simplify
   , eval
   , degree
   , getInt
   , getRat
   , isInt
   , isRat)
where
import GHC.Generics (Generic)
import Data.Data
import Data.Typeable()
import Data.Generics.Uniplate.Direct
import qualified Polynomial.MultiPoly as MP 
import Polynomial.MultiPoly (MultiPoly)
import Polynomial.SinglePoly () 
import SymbolRoot.SymbolRoot (SymbolRoot)
import qualified SymbolRoot.SymbolRoot as SR
import qualified Data.Map as Map
import Utils.Num2Frac
import Utils.Utils (getFix, showRat)
import qualified Utils.Utils as U (isInt)
import Data.Maybe (isJust)

data ExprRoot = HighRoot Integer ExprRoot
              | N Rational
              | ExprRoot :+: ExprRoot 
              | ExprRoot :*: ExprRoot 
              | ExprRoot :/: ExprRoot 
   deriving (Eq, Ord, Data, Typeable, Generic)

instance Uniplate ExprRoot where
   uniplate (N a)          = plate N |- a  
   uniplate (HighRoot n a) = plate HighRoot |- n  |* a
   uniplate (a :+: b)      = plate (:+:) |* a |* b
   uniplate (a :*: b)      = plate (:*:) |* a |* b
   uniplate (a :/: b)      = plate (:/:) |* a |* b

instance Show (ExprRoot) where 
   show r = let br s = case s of 
                   _:+:_ -> "(" ++ show s ++ ")" 
                   _ -> show s 
            in 
      case r of 
      a :+: b -> "(" ++ show a ++ " + " ++ show b ++ ")"
      a :*: b -> br a ++ " * " ++ br b
      a :/: b | b == 1 -> show a
              | otherwise -> br a ++ " / " ++ br b 
      N a -> showRat a 
      HighRoot n a | n == 1 -> show a 
                   | n == 2  -> "sqrt(" ++ show a ++ ")" 
                   | otherwise -> show n ++ "^√(" ++ show a ++ ")" 

eval :: ExprRoot -> Double
eval r = case r of 
   HighRoot n a -> (eval a) ** (1 / fromInteger n) 
   N a -> fromRational a
   a :+: b -> eval a + eval b
   a :*: b -> eval a * eval b
   a :/: b -> eval a / eval b

sign :: ExprRoot -> ExprRoot
sign r
   | eval r > 0  = N 1
   | eval r < 0  = N (-1)
   | otherwise   = N 0

toMulti :: 
      ExprRoot -> Frac (MultiPoly ExprRoot Rational)
toMulti q = case q of 
   HighRoot _ _ -> toFrac (MP.var q) 1
   N a -> toFrac (MP.constant a) 1
   a :+: b -> toMulti a + toMulti b
   a :*: b -> toMulti a * toMulti b
   a :/: b -> toMulti a / toMulti b

fromMulti :: 
      Frac (MultiPoly ExprRoot Rational) -> ExprRoot
fromMulti q = let n = numerator q 
                  d = denominator q
                  c = MP.constant $ 1 / MP.leadCoeff d
               in if d == 1 then g n else  (g $ c * n) :/: (g $ c * d)
   where 
      g p = foldr1 (:+:) $ do 
         (ps, a) <- (MP.getTerms p)
         if (null ps) then return (N a) else do 
            let term = foldr1 (:*:) $ map f $ Map.toList ps
            if a == 1 then return term else return (N a :*: term)
         where f (x, n) 
                  | n == 0 = 1
                  | n == 1 = x 
                  | otherwise = foldr1 (:*:) (take n $ repeat x) 

keepRoots :: 
   ExprRoot -> ExprRoot 
keepRoots = fromMulti . toMulti

degree :: ExprRoot -> Integer
degree r = case r of 
   a :*: b -> max (degree a) (degree b) 
   a :+: b -> max (degree a) (degree b)
   a :/: b -> max (degree a) (degree b)
   HighRoot n _ -> n
   N _ -> 0

instance Num ExprRoot where 
   (a :+: b) + c = a + (b + c)
   a + b = keepRoots $ a :+: b
   (a :*: b) * c = a * (b * c)
   a * b = keepRoots $ a :*: b
   negate a = keepRoots $ N (-1) * a
   signum = sign
   abs a = keepRoots $ (sign a) * a
   fromInteger = N . fromInteger

instance Fractional ExprRoot where
   a / b = keepRoots $ a :/: b  
   fromRational = N 

toSymbol :: ExprRoot -> Maybe SymbolRoot
toSymbol q = case q of 
   HighRoot n (N a) -> Just $ SR.highRoot n a
   HighRoot n (HighRoot m a) -> toSymbol $ HighRoot (n * m) a
   HighRoot _ _ -> Nothing
   N a -> Just $ SR.constant a
   a :+: b -> (+) <$> toSymbol a <*> toSymbol b
   a :*: b -> (*) <$> toSymbol a <*> toSymbol b
   a :/: b -> f a b 
   where 
      f x y = case toSymbol y of 
         Just y' | y == 0 -> Nothing
                 | SR.numberOfTerms y' == 1 -> (/) <$> toSymbol x <*> toSymbol y
                 | SR.degree y' > 15 -> Nothing
                 | SR.numberOfTerms y' > 3 -> Nothing
                 | otherwise -> (/) <$> toSymbol x <*> toSymbol y
         _ -> Nothing

fromSymbol :: SymbolRoot -> ExprRoot
fromSymbol r =
   foldr1 (+) $ do 
      ((p, a), b) <- SR.getValues r
      let g  | p == 1 = N $ fromInteger a * b
             | b == 1 = HighRoot p (N $ fromInteger a)
             | otherwise = N b * HighRoot p (N $ fromInteger a)
      return g 

simplify :: ExprRoot -> ExprRoot
simplify = getFix $ transform f 
   where f a = maybe a fromSymbol $ toSymbol a

constant :: Rational -> ExprRoot
constant a = N a

highRoot :: Integer -> ExprRoot -> ExprRoot
highRoot n a 
   | n == 0 = 1
   | n == 1 = a 
   | a >= 0 = HighRoot n $ keepRoots a
   | otherwise = error "only defined for positive values"

sqRoot :: ExprRoot -> ExprRoot
sqRoot = highRoot 2

getInt :: ExprRoot -> Maybe Integer
getInt r =  case simplify r of 
   N r' | U.isInt r' -> Just $ round r'
   _  -> Nothing

getRat :: ExprRoot -> Maybe Rational
getRat r = case simplify r of 
   N r' | not (U.isInt r') -> Just r'
   _ -> Nothing

isRat :: ExprRoot -> Bool
isRat = isJust . getRat

isInt :: ExprRoot -> Bool
isInt = isJust . getRat