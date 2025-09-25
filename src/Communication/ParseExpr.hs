module Communication.ParseExpr where 
import Text.Parsec
import Text.Parsec.Pos (newPos)
import Text.Parsec.Error -- (messageString, errorMessages)
import Text.Parsec.String (Parser)
import Text.Parsec.Token hiding 
   (integer, natural, parens, identifier, symbol, whiteSpace)
import qualified Text.Parsec.Token as T 
import Text.Parsec.Language (haskellDef)
import Polynomial.ExprPoly 
   ( ExprPoly, PolyEq ((:=:)), simplify, keepFactors
   , (.^.), mapTerms, OrList, list2or)
import SymbolRoot.ComplexRoot (ComRoot)
import qualified SymbolRoot.ComplexRoot as CR
import qualified Polynomial.ExprPoly as EP   
import Utils.Utils (replace, replaceFirst, rounder)

-- Define the Expr datatype
data Expr = Expr :+: Expr
          | Expr :-: Expr
          | Expr :*: Expr
          | Expr :/: Expr
          | Negate Expr
          | Sqrt Expr
          | Expr :^: Expr
          | N Integer
          | D Double
          | Var String
   deriving (Eq, Show)

lexer :: TokenParser ()
lexer = makeTokenParser haskellDef

whiteSpace :: Parser ()
whiteSpace = T.whiteSpace lexer

integer :: Parser Integer
integer = --do{symbol "-"; n <- natural; return (-n)} 
          -- <|> 
          natural 
   where natural = T.natural lexer

double :: Parser Double
double = --do {symbol "-"; n <- double'; return (-n)} 
         -- <|> 
         double'
   where double' = float lexer

identifier :: Parser String
identifier = T.identifier lexer

negIdentifier :: Parser String
negIdentifier = do symbol "-"
                   identifier

parens :: Parser a -> Parser a
parens = T.parens lexer

symbol :: String -> Parser String
symbol = T.symbol lexer 

mixedFraction :: Parser Expr
mixedFraction = do a <- integer
                   symbol "%3B"
                   n <- integer
                   symbol "/"
                   d <- integer
                   return (N (a*d+n):/:N d)

sqrtP :: Parser Expr
sqrtP = do symbol "sqrt"
           x <- parens expr
           return (Sqrt x)

sqrtN :: Parser Expr
sqrtN = do symbol "-"
           symbol "sqrt"
           x <- parens expr
           return (N(-1) :*: Sqrt x)

neg :: Parser Expr -> Parser Expr
neg ex = do symbol "-"
            x <- ex
            return (Negate x) 

expr :: Parser Expr
expr = term `chainl1` addop

term :: Parser Expr
term = expo `chainl1` mulop

expo :: Parser Expr
expo = factor `chainr1` expop

atom :: Parser Expr
atom = try (mixedFraction)
      <|> try sqrtP
      <|> try (D <$> double)
      <|> try (N <$> integer)
      <|> try (Var <$> symbol "x")
      <|> try (Var <$> symbol "D")
      <|> try (Var <$> identifier)

factor :: Parser Expr
factor =  try atom 
          <|> try (parens expr) 
          <|> try (neg atom)
          <|> try (neg (parens expr))

expop :: Parser (Expr -> Expr -> Expr)
expop = do {symbol "^"; return (:^:)} 

mulop :: Parser (Expr -> Expr -> Expr)
mulop = do{ symbol "*"; return (:*:) }
    <|> do{ symbol "/"; return (:/:) }
    <|> do{ symbol ""; notFollowedBy (symbol "-"); return (:*:) }

addop :: Parser (Expr -> Expr -> Expr)
addop = do{ symbol "+"; return (:+:) }
    <|> do{ symbol "-"; return (:-:) }

parseExpr :: String -> Either String Expr
parseExpr s = case parse ( whiteSpace *> expr <* eof) "" s of 
   Left _  ->  Left varMsg
   Right p ->  Right p

makePoly :: Expr -> Either String (ExprPoly String ComRoot)
makePoly p = let simp = fmap CR.simplify . simplify  
   in case p of 
    a :+: b -> (+) <$> makePoly a <*> makePoly b
    a :-: b | a == N 0 -> makePoly (Negate b)
            | otherwise -> (-) <$> makePoly a <*> makePoly b
    a :*: b -> (*) <$> makePoly a <*> makePoly b
    N a :/: N b -> Right $ EP.N((fromIntegral a)/(fromIntegral b))
    a :/: b -> case simp <$> makePoly b of 
                  Right (EP.N r) | r /= 0 -> 
                     (mapTerms (EP.N (CR.simplify $ 1/r)*)) <$> (makePoly a)
                  _ -> Left divMsg
    a :^: b -> case b of N n -> (.^. (fromIntegral n)) <$> (makePoly a)
                         _ -> Left expMsg
    Sqrt a -> case simp <$> makePoly a of 
                  Right (EP.N r) | r /= 0 -> Right $ EP.N (CR.sqRoot r)
                  _  -> case keepFactors <$> makePoly a of 
                     Right (x EP.:^: n) 
                        | even n -> Right (x EP.:^: (round $ (fromIntegral n)/2)) 
                     _ -> Left sqrtMsg
    Negate (N a) -> Right $ EP.N $ fromIntegral $ negate a
    Negate (N a :/: b) -> makePoly $ (N $ fromIntegral $ negate a) :/: b
    Negate a -> negate <$> makePoly a
    N a -> Right $ EP.N $ fromIntegral a
    D d -> Right $ EP.N $ fromRational $ rounder 8 d
    Var x | x == "x" 
          || x == "D" -> Right $ EP.Var x
          | otherwise -> Left varMsg

divMsg  = "Je deelt door een formule, dat is bij deze opgaven niet nodig"
expMsg  = "Je zet een formule, berekening of breuk in de exponent, \
          \dat is bij deze opgaven niet nodig"
sqrtMsg = "Bij deze opgaven kan de wortel van een formule alleen voorkomen \
          \als de formule een kwadraat is"
varMsg = "Je gebruikt een functie of een variabele die bij deze opgaven niet nodg is"
constMsg = "Er zit geen variabele in een van je vergelijkingen, \
          \als je een oplossing wil invoeren noteer dan zo: x = ..."

repairStartMinus :: String -> String
repairStartMinus = replaceFirst "-" "0-"

noWhite :: String -> String
noWhite = replace " " ""

repairSquare :: String -> String
repairSquare  = replace "²" "^2" 

repairSum :: String -> String
repairSum = replace "%2B" "+"

parse2poly :: String -> Either String (ExprPoly String ComRoot)
parse2poly s = 
      case parseExpr 
           $ repairStartMinus 
           $ repairSum 
           $ noWhite s 
      of Right p  -> makePoly p  
         Left msg -> Left msg

parse2eq :: String -> Either String (PolyEq String ComRoot)
parse2eq p = 
   if not(('x' `elem` p) || ('D' `elem` p)) 
   then Left constMsg 
   else case break (=='=') p of 
           (lhs,'=':rhs) -> (:=:) <$> (parse2poly lhs) <*> (parse2poly rhs) 
           _ -> Left "not an equation"

breakAt :: String -> String -> (String, String)
breakAt _ [] = ([],[])
breakAt (x:xs) p = let (l, r) = break (==x) p in 
   if take n r == (x:xs) then (l, drop n r) 
   else let (l', r') = breakAt (x:xs) (drop n r) 
        in (l ++ take n r ++ l', r')
   where n = length (x:xs)

parse2or :: String -> Either String (OrList String ComRoot)
parse2or p = case breakAt "of" p of 
   ("",_) -> Left "invalid input"
   (s,"") -> (list2or . (:[])) <$> (parse2eq s) 
   (s, t) -> f <$> (parse2eq s) <*> (parse2eq t)
      where f x y = list2or [x, y]   