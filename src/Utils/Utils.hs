module Utils.Utils 
 ( getFix
 , isInt
 , showRat
 , iterN
 , setNub
 , replace
 , replaceFirst
 , copy
 , rounder
 , roundDown
 , roundUp) where
import qualified Data.List as L
import qualified Data.Set as S
import Data.Ratio 

setNub :: Ord a => [a] -> [a]
setNub = S.toAscList . S.fromList

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace _ _ [] = []
replace old new (x:xs) = 
   if take n (x:xs) == old 
   then new ++ replace old new (drop n (x:xs)) 
   else x:replace old new xs 
   where n = length old

replaceFirst :: Eq a => [a] -> [a] -> [a] -> [a]
replaceFirst _ _ [] = []
replaceFirst old new (xs) = 
   if take n xs == old 
   then new ++ drop n xs
   else xs
   where n = length old

getFix :: Eq a => (a -> a) -> a -> a
getFix f a 
   | new == a  = a
   | otherwise = getFix f new
      where new = f a

isInt :: Rational -> Bool
isInt t = (fromIntegral (round t :: Integer)) == t

showRat :: Rational -> String
showRat r = 
   if isInt r 
   then show (round r :: Integer) 
   else show (numerator r) ++ "/" ++ show (denominator r)
            

iterN :: Int -> (a -> a) -> a -> a
iterN n f = last . take (n + 1) . L.iterate f 

copy :: a -> a
copy = id

rounder :: RealFrac a => Int -> a -> Rational
rounder k = (/10^k) . round' . (* 10^k) 
   where 
      round' b = fromIntegral $ 
                 if abs r < 0.5 
                 then n 
                 else n + signum n 
         where (n, r) = properFraction b

roundDown :: RealFrac a => Int -> a -> Rational
roundDown k = (/10^k) . fromIntegral . floor . (* 10^k) 

roundUp :: RealFrac a => Int -> a -> Rational
roundUp k = (/10^k) . fromIntegral . ceiling . (* 10^k) 

