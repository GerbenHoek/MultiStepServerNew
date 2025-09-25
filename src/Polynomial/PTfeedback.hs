module Polynomial.PTfeedback where 
import Diagnosis.RelationTree 
import qualified Polynomial.PolyRelations as R
import Polynomial.ExprPoly (emptyOr)

data PTmsg = PF { event :: [String] -> [String] -> Bool 
                , msg   :: String }

message :: 
   [Test a] -> [Test a] -> String -> PTmsg
message s u = PF (\st us -> (all (`elem` st) s') && (all (`elem` us) u'))
   where 
      s' = map testName s 
      u' = map testName u

expectedDisc :: PTmsg
expectedDisc = message 
   [] [R.expectedDisc emptyOr] 
      "Deze vergelijking kun je zonder de abc-formule oplossen"

noZeroDegree :: PTmsg
noZeroDegree = message 
   [] [R.noZeroDegree emptyOr]
      "Je hebt een overbodige vergelijking opgeschreven"

biggerNrEqs :: PTmsg
biggerNrEqs = message
   [R.biggerNrEqs emptyOr]
   [R.expectedNrEqs emptyOr]
      "Je hebt een overbodige vergelijking opgeschreven"

smallerNrEqs :: PTmsg
smallerNrEqs = message
   []
   [ R.biggerNrEqs emptyOr
   , R.expectedNrEqs emptyOr]
      "Je hebt onverwacht twee vergelijkingen samengevoegd"

expectedNormalFac :: PTmsg
expectedNormalFac = message 
   [R.obtainedByDist emptyOr emptyOr] 
      [R.expectedNormal emptyOr]
         "Je hebt onverwacht de haakjes uitgewerkt"
         
expectedNormalDist :: PTmsg
expectedNormalDist = message 
   []
   [ R.expectedNormal emptyOr 
   , R.obtainedByDist emptyOr emptyOr] 
      "Je hebt op een onverwachte manier ontbonden in factoren \
       \(extra haakjes gemaakt)"

expectedNrOfTerms :: PTmsg
expectedNrOfTerms = message
   [] [R.expectedNrOfTerms emptyOr] 
      "Je kunt nog termen samenvoegen"

expectedZeroZero :: PTmsg
expectedZeroZero = message
   []
   [ R.expectedZero emptyOr
   , R.allDerivedToZero emptyOr] 
      "De vergelijking zou op nul herleid moeten zijn"

expectedZeroNzero :: PTmsg
expectedZeroNzero = message
   [R.allDerivedToZero emptyOr] 
   [R.expectedZero emptyOr]
      "Je hebt onverwacht op nul herleid"

expectedSquares :: PTmsg
expectedSquares = message
   [] [R.expectedSquares emptyOr]
      "Je hebt onverwacht de kwadraat uitgewerkt"

expectedLeadNDiv :: PTmsg
expectedLeadNDiv = message
   []
   [ R.expectedLead emptyOr
   , R.obtainedByDist emptyOr emptyOr]
      "Je hebt de vergelijking onverwacht \
      \vermenigvuldigd of gedeelt"

expectedLeadDiv :: PTmsg
expectedLeadDiv = message 
   [R.obtainedByDist emptyOr emptyOr]
   [R.expectedLead emptyOr]
   "Je hebt voor een constante factor de haakjes uitgwerkt, \
   \maar we verwachten dat je zou delen door deze factor"

getPTFeedback :: [String] -> [String] -> String
getPTFeedback st us = 
   case lookup True [(e st us, s) | PF e s <- msgs]
   of Nothing -> err 
      Just s' -> s'
   where
      err = "Je volgt niet de verwachte strategie"
      msgs = 
         [ noZeroDegree 
         , expectedDisc
         , biggerNrEqs
         , smallerNrEqs
         , expectedNormalFac
         , expectedNormalDist
         , expectedNrOfTerms
         , expectedZeroZero
         , expectedZeroNzero
         , expectedSquares
         , expectedLeadNDiv
         , expectedLeadDiv]