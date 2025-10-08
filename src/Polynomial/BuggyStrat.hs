module Polynomial.BuggyStrat where 
import Regulation.Search
import Regulation.Focus
import Regulation.Lift ((>>>))
import FSMMaker.Rules() 
import FSMMaker.StrategyBas hiding (applyAll)
import Polynomial.PolyLift 
import Polynomial.PolyRules
import Polynomial.PolyBuggys
import Polynomial.PolyStrat (evalAndZero)
import SymbolRoot.ComplexRoot hiding (eval)

workoutSquareB :: Strat (Focus (PEX ComRoot))
workoutSquareB = 
   R workoutSquareB1 
   <|> R workoutSquareB2 
   <|> R workoutSquareB3 
   <|> R workoutSquareB4 
   <|> R workoutSquareB5

abcStratB :: Strat (Focus (PQS ComRoot))
abcStratB = 
   R allDeg2
   -- .*. manyN 2 (focus (tlf >>> tlt >>> x2s) (R negateTerm) .*. R nubSub)
   .*. (repeatS (focus (x2x >>> x2s) workoutSquareB)
       <|> repeatS (focus (x2x >>> x2s) (R workoutSquare)))
   .*. (repeatS (focus (x2x >>> x2s) (R distributeB))
       <|> repeatS (focus (x2x >>> x2s) (R distribute) .*. R nubSub))
   .*. try (R rebracket)
   .*. manyN 2 (focus (tlt >>> x2s) (R negateTerm) .*. R nubSub)
   .*. manyN 2 (focus (x2x >>> x2s) (R squareB))
   .*. try (focus e2s (R derive2zero))
   .*. try (R eval)
   .*. R wait
   .*. R nubSub
   -- .*. option (focus e2s (R derive2zero 
   --                       .*. (R factorLead <|> R factorDivisor) 
   --                       .*. R divideByFirst))
   .*. (R abcAlways 
        <|> R abcDivideBy_a 
        <|> R abcForgetRoot 
        <|> R abcForgetSquareB 
        <|> R abcNegativeSquareB
        <|> focus e2s (R discWithPlus 
                       <|> R discForgetSquareB 
                       <|> R discNegativeSquareB))



divisionB :: Strat (Focus (PEQ ComRoot))
divisionB = 
   R divideByFirst 
   <|> R divideReverse 
   <|> R divideSubtract 
   <|> R divideForget

linSquareStratB :: Strat (Focus (PQS ComRoot))
linSquareStratB = 
      try (R eval)   .*.
      focus e2s (option (R flipEq) .*. try (R transportConstant)    
                    .*. option divisionB)
                    .*. (R linSquare
                         <|> R linSquareForgetRoot 
                         <|> R linSquareForgetRootConst
                         <|> focus e2s (R removeSquareB))
     .*. option approximateRootsStat

nillPruductStratB :: Strat (Focus (PQS ComRoot))
nillPruductStratB = 
      option evalAndZero .*.
      focus e2s (option (R flipEq) .*. option (R transportConstant) 
                 .*. option divisionB)
                 .*. (R nillProduct <|> R nillProductB)
                 .*. try (focus s2s (R removeDegen))

linStratB :: Strat (Focus (PQS ComRoot))
linStratB = 
   R allLin 
   .*. (repeatS (focus (x2x >>> x2s) (R distribute)) 
       <|> repeatS (focus (x2x >>> x2s) (R distributeB)))
   .*. try (R rebracket)
   .*. manyN 2 (focus (tlt >>> x2s) (R negateTerm) .*. R nubSub)
   .*. R sortAllLin
   .*. repeatS (focus e2s divisionB)

approximateRootsStat :: Strat (Focus(PQS ComRoot))   
approximateRootsStat = 
   R (approximateRoots 1) 
   <|> R (approximateRoots 2) 
   -- <|> R (approximateRoots 3) 

roundSolStrat :: Strat (Focus(PQS ComRoot)) 
roundSolStrat = 
    R (roundSol 1) <|> R (roundSol 2) -- <|> R (roundSol 3)

buggyStrat :: Strat (Focus (PQS ComRoot))
buggyStrat = 
   (linStratB <|> 
      abcStratB <|> (linSquareStratB <|> nillPruductStratB) .*. linStratB)
     .*. R wait 
     .*. R nubSub
     .*. try (R simplifyAllRoots)
     .*. option roundSolStrat
     .*. R wait 
     .*. R nubSub
     .*. option (R forgetEquation) 
     .*. manyN 2 (focus e2s (R negateSolution))