module Polynomial.PTHint where
import FSMMaker.Rules (Rule, ruleName)
import qualified Polynomial.PolyRules as R

data PTHint = PH {event :: [String] -> Bool, msg :: String}

hint :: Rule a -> String -> PTHint
hint r = PH ((ruleName r) `elem`) 

distributePath :: PTHint
distributePath = hint R.distributePath 
   "De verwachte strategie was om haakjes weg te werken."

nillProductPath :: PTHint
nillProductPath = hint R.nillProductPath 
   "De verwachte strategie was om gebruik te \ 
   \maken van de regel: <br> \"A*B = 0 => A = 0 of B = 0\"."

linSquarePath :: PTHint
linSquarePath = hint R.linSquarePath 
   "De verwachte strategie was om zo nodig door een \
   \constante te delen en daarna aan beide kanten van de \
   \vergelijking de wortel te nemen."

factorTwoTermPath :: PTHint
factorTwoTermPath = hint R.factorTwoTermPath 
   "De verwachte strategie was om zo nodig \
   \op nul te herleiden en x buiten de haakjes te brengen."

factorThreeTermPath :: PTHint
factorThreeTermPath = hint R.factorThreeTermPath    
   "De verwachte strategie was om zo nodig \
   \op nul te herleiden en te ontbinden in factoren."

completeSquarePath :: PTHint
completeSquarePath = hint R.completeSquarePath 
   ""

doubleFactorPath :: PTHint
doubleFactorPath = hint R.doubleFactorPath
   "De verwachte strategie was om gebruik te \ 
   \maken van de regel: <br> \"A*B = A*C => A = 0 of B = C\"."

abcPath :: PTHint
abcPath = hint R.abcPath 
   "De verwachte strategie was om zo nodig \
   \op nul te herleiden en de abc-formule te gebruiken."

linPath :: PTHint
linPath = hint R.linPath 
   "De verwachte strategie was om de \
   \lineaire vergelijking(en) op te lossen."

dividePath :: PTHint
dividePath = hint R.dividePath
   "De verwachte strategie was om \
   \als eerste door een constante te delen." 

getPTHint :: [String] -> String
getPTHint p = 
   case lookup True [(e p, s) | PH e s <- msgs]
   of Nothing -> err 
      Just s' -> s'
   where
      err = ""
      msgs = 
         [ distributePath
         , factorTwoTermPath 
         , factorThreeTermPath
         , nillProductPath
         , doubleFactorPath
         , abcPath
         , linPath
         , linSquarePath]