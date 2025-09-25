module Polynomial.PolyTasks where 
import SymbolRoot.ComplexRoot (ComRoot)
import Polynomial.ExprPoly
import Polynomial.BuggyStrat (buggyStrat)
import FSMMaker.FSM (fsm)
import Polynomial.PolyLift (PQS(..))
import Regulation.Focus (embed, top)
import Regulation.Search (applyAll)
import Diagnosis.MBTdiagnosis (countBuggy)
import Regulation.Tracer (buggy2tracer, element)
import Data.Set (size, fromList)
import System.Random (randomR, randomRs, mkStdGen, RandomGen, randomIO)
import Data.List (sortBy)
import Polynomial.WorkedExample (showEq)
import Communication.ParseExpr (parse2eq)
import Utils.Utils (setNub)

type Task = [ComRoot] -> Maybe (OrList String ComRoot)

nillProduct2 :: Task
nillProduct2 [a, b, c, d] = Just $ list2or 
   [(constant a * var "x" + constant b) * (constant c * var "x" + constant d) :=: 0] 
nillProduct2 _ = Nothing

nillProduct :: Task
nillProduct [a, b] = Just $ list2or 
   [(var "x" + constant a) * (var "x" + constant b) :=: 0] 
nillProduct _ = Nothing

twoTerm :: Task
twoTerm [a,b] = Just $ list2or 
   [constant a * var "x" .^. 2 + constant b * var "x" :=: 0] 
twoTerm _ = Nothing

linSquare :: Task
linSquare [a, b, c, d] = 
   if d < 0 then Nothing else Just $ list2or 
   [constant a * (constant b * var "x" + constant c).^.2 :=: constant d] 
linSquare _ = Nothing

factorThreeTerm :: Task
factorThreeTerm [a,b] = 
   if a + b == 0 then Nothing else Just $ list2or 
   [simplify ((var "x" + constant a) * (var "x" + constant b)) :=: 0] 
factorThreeTerm _ = Nothing

factorize :: Task
factorize [a, b, c, d, e, f] = 
  let eq = (constant a * var "x" + constant b) 
           * (constant c * var "x" + constant d) 
           :=: 
           (constant a * var "x" + constant b) 
           * (constant e * var "x" + constant f) 
   in case discriminant eq of 
         [ _ :=: N dis] | dis > 0 -> Just $ list2or [eq]
         _ -> Nothing
factorize _ = Nothing

nrFinals :: OrList String ComRoot -> Int
nrFinals = size . fromList . 
   map (top . element . fst) .
   applyAll 
      (fsm $ fmap (countBuggy 3 . buggy2tracer) buggyStrat) 
   . embed . PQS 

parameterSearch :: [OrList String ComRoot] -> [(Int, OrList String ComRoot)]
parameterSearch ps = 
   take 75 $ sortBy f $ zip (map nrFinals ps) ps  
   where f x y = compare (fst y) (fst x)

randomInts :: Int -> [Int]
randomInts n = (randomRs (0, 10^6) (mkStdGen n) :: [Int])

pickRandom :: RandomGen g => g -> [a] -> (a, g)
pickRandom g as = 
   let (n, g') = randomR (0, length as - 1) g 
   in (as !! n, g')

pickRandoms :: RandomGen g => g -> [[a]] -> ([a], g)
pickRandoms g []  = ([], g)
pickRandoms g ass = 
   let (as, gs) = unzip $ pickRandoms' g ass in 
   (as, last gs)
   where 
      pickRandoms' _ [] = []
      pickRandoms' g (as:ass) = 
         let (a', g') = pickRandom g as in 
         (a', g') : pickRandoms' g' ass

pickRandomN :: Ord a => Int -> Int -> [[a]] -> [[a]]
pickRandomN g n = take n . pickRandomN' (mkStdGen g) 
   where 
      pickRandomN' g ass = 
         let (as', g') = pickRandoms g ass in 
         as':pickRandomN' g' ass
   
baseList :: Int -> Int -> [[ComRoot]] 
baseList k n = 
   let cs = map fromIntegral $ [(-n)..(-1)] ++ [1..n] in 
   take k $ repeat cs  


baseListP k n = 
   let cs = map fromIntegral $ [1..n] in 
   take k $ repeat cs

nillProductT :: Int -> [OrList String ComRoot]
nillProductT k = do 
   Just eq <- take k 
              $ setNub 
              $ map nillProduct 
              $ pickRandomN 2025 (2*k) 
              $ baseList 2 9
   return eq
    
twoTermT :: Int -> [OrList String ComRoot]
twoTermT k = do 
   Just eq <- take k 
              $ setNub 
              $ map twoTerm 
              $ pickRandomN 2025 (2*k) 
              $ (baseListP 1 9 ++ baseList 1 9)
   return eq

linSquareT :: Int -> [OrList String ComRoot]
linSquareT k = do 
   Just eq <- take k 
              $ setNub 
              $ map linSquare 
              $ pickRandomN 2025 (2*k) 
              $ (baseListP 1 9 ++ baseListP 1 9 ++ baseList 1 9 ++ baseListP 1 9)
   return eq

factorThreeTermT :: Int -> [OrList String ComRoot] 
factorThreeTermT k = do 
   Just eq <- take k 
              $ setNub 
              $ map factorThreeTerm 
              $ pickRandomN 2025 (2*k) 
              $ baseList 2 9
   return eq

factorizeT :: Int -> [OrList String ComRoot] 
factorizeT k = do 
   Just eq <- take k 
              $ setNub 
              $ map factorize 
              $ pickRandomN 2025 (2*k) 
              $ (baseListP 2 9 ++ baseList 1 9 
                 ++ baseListP 2 9 ++ baseList 1 9)
   return eq

writeParameters :: FilePath -> [OrList String ComRoot] -> IO()
writeParameters fp ps = 
   writeFile fp $ show $ [ (n, show p') 
                         | (n, p) <- parameterSearch ps
                         , p' <- or2list $ showEq p] 

getTask
  :: FilePath -> IO [String]
getTask fp = do
   s <- readFile fp 
   let ps = read s :: [(Int, String)]
       ps' = map snd ps
   return ps'  

randomN :: Int -> Int -> IO Int
randomN l h = do 
   rn <- randomIO :: IO (Int)
   let rn' = rn `mod` (h-l)
   return (l + rn') 

randomTask :: Int -> IO String
randomTask n = do 
   k <- randomN 0 74
   s <- getTask ("./Parameters/" ++ file)
   let t = s !! k
   return t
      where 
         file 
            | n == 0 = "nillProduct"
            | n == 1 = "twoTerm"
            | n == 2 = "linSquare"
            | n == 3 = "factorThreeTerm"
            | n == 4 = "factorize"
            | otherwise = error "file not found"

loadTasks :: IO [String]
loadTasks = mapM randomTask [0..4]