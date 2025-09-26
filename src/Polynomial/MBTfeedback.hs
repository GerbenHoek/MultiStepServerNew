module Polynomial.MBTfeedback where 
import FSMMaker.Rules
import qualified Polynomial.PolyBuggys as B

data MBTmsg = MF { event :: [[String]] -> Bool 
                 , msg   :: String }

flatMessage :: [Rule a] -> String -> MBTmsg
flatMessage rs = MF 
   (\mbt -> any (\r -> (ruleName r) `elem` (concat mbt)) rs) 

negateTerm :: MBTmsg 
negateTerm = 
   flatMessage [B.negateTerm]
      "Je hebt een fout gemaakt met een minteken. \
      \Dit kan bijvoorbeeld ook gebeuren als \
      \je een term naar de andere kant van het \
      \=-teken brengt."

forgetEquation :: MBTmsg
forgetEquation = 
   flatMessage [B.forgetEquation]
      "Je bent een oplossing kwijtgeraakt. <br>\
      \Dit kan bijvoorbeeld gebeuren als je deelt door een \
      \factor en vergeet dat deze ook nul mag zijn. \
      \Of als je de negatieve wortel vergeet bij worteltrekken."

workoutSquareB :: MBTmsg
workoutSquareB =
   flatMessage 
      [ B.workoutSquareB1
      , B.workoutSquareB2
      , B.workoutSquareB3
      , B.workoutSquareB4]
         "Let op: <br> \
         \(a + b)² <br>\
         \= (a+b) * (a+b) <br> \
         \= a² + 2ab + b²"

workoutSquareB5 :: MBTmsg
workoutSquareB5 = 
   flatMessage [B.workoutSquareB5]
      "Let op: <br> \
         \(a + b)² <br>\
         \= (a+b) * (a+b) <br> \
         \= a² + 2ab + b² <br><br>\
         \ Jij hebt \"a*b\" gebruikt in plaats van \"2ab\"." 

squareB :: MBTmsg
squareB = 
   flatMessage [B.squareB]
      "Je hebt een getal keer 2 gedaan \
      \in plaats van het te kwadrateren."


distributeB :: MBTmsg
distributeB = 
   flatMessage 
      [B.distributeB]
         "Als je een factor voor de haakjes uitwerkt moet je alle \ 
         \termen met die factor vermenigvuldigen."

divideReverse :: MBTmsg
divideReverse = 
   flatMessage [B.divideReverse]
      "Je deelt verkeerd om."

divideWrong :: MBTmsg 
divideWrong = 
   flatMessage 
      [ B.divideForget
      , B.divideSubtract]
         "Als je aan de ene kant van het =-teken deelt, \
         \dan moet je dat aan de andere kant ook doen."

nillProductB :: MBTmsg
nillProductB = 
   flatMessage [B.nillProductB]
      "De regel: \"A*B = 0 dan A = 0 of B = 0\" \
      \geldt alleen voor het getal 0, \
      \maar niet voor andere getallen."

roundRoots :: MBTmsg
roundRoots = 
   flatMessage 
      [ B.roundSol 0
      , B.approximateRoots 0]
         "Geef de berekeningen exact, \
         \laat dus wortels en breuken staan als ze \
         \niet mooi uitkomen."

linSquareForgetRoot :: MBTmsg
linSquareForgetRoot =
   flatMessage [B.linSquareForgetRoot]
      "Als je aan de ene kant van het =-teken \
      \de wortel neemt, moet je dat aan de andere kant ook doen."

linSquareForgetRootConst :: MBTmsg
linSquareForgetRootConst = 
   flatMessage [B.linSquareForgetRootConst]
      "Als je de wortel neemt, dan moet je die van alle factoren nemen."

abcForgetRoot :: MBTmsg
abcForgetRoot = 
   flatMessage [B.abcForgetRoot]
      "Je bent de wortel van de discriminant vergeten te nemen."

abcDivideBy_a :: MBTmsg
abcDivideBy_a = 
   flatMessage [B.abcDivideBy_a]
      "Je hebt in de abc-formule door \"a\" gedeeld in plaats van door \"2a\"."

abcForgetSquareB :: MBTmsg
abcForgetSquareB =
   flatMessage [ B.abcForgetSquareB
               , B.discForgetSquareB] 
      "Je bent veregeten dat b in het kwadraat moet \
      \bij het berekenen van de discriminant."

abcNegativeSquareB :: MBTmsg
abcNegativeSquareB =
   flatMessage [ B.abcNegativeSquareB
               , B.discNegativeSquareB]
      "Als je b kwadrateert wordt de uitkomst positief."

discWithPlus :: MBTmsg
discWithPlus = 
   flatMessage [B.discWithPlus]
      "Je hebt een fout gemaakt met een minteken."

getMBTFeedback :: [[String]] -> String
getMBTFeedback mbt = 
   case lookup True [(e mbt, s) | MF e s <- msgs]
   of Nothing -> err 
      Just s' -> s'
   where
      err = "Er is iets mis misgegaan in je berekening."
      msgs = [ workoutSquareB 
             , workoutSquareB5
             , squareB
             , linSquareForgetRoot
             , linSquareForgetRootConst
             , nillProductB 
             , divideReverse
             , divideWrong
             , distributeB
             , abcForgetRoot
             , abcDivideBy_a
             , abcForgetSquareB
             , abcNegativeSquareB
             , discWithPlus
             , negateTerm 
             , roundRoots
             , forgetEquation]