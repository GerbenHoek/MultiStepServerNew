module FSMMaker.Rules 
   ( Rule
   , rule
   , defaultRule
   , buggyRule  
   , minorRule
   , isBuggy
   , isMinor
   , makeMinor
   , pickOne
   , ruleName
   , applyRule
   , notR
   , test
   , path) where 

data Rule a = 
   Rule { isBuggy   :: Bool
        , isMinor   :: Bool
        , ruleName  :: String
        , applyRule :: a -> [a]}

instance Show (Rule a) where 
   show r = ruleName r

defaultRule :: Rule a
defaultRule = Rule False False "" (:[])

rule :: String -> (a -> [a]) -> Rule a
rule = Rule False False 

buggyRule :: String -> (a -> [a]) -> Rule a
buggyRule = Rule True False

minorRule :: String -> (a -> [a]) -> Rule a
minorRule = Rule False True 

makeMinor :: Rule a -> Rule a
makeMinor r = r {isMinor = True}

pickOne :: Rule a -> Rule a
pickOne r = r {applyRule = f}
  where f a = case applyRule r a of
           a':_ -> [a']
           _ -> []

notR :: Rule a -> Rule a 
notR (Rule _ _ s f) = minorRule s' f' 
   where 
      f' a = case f a of 
         [] -> [a]
         _  -> []
      s' = "not " ++ s

test :: (a -> Bool) -> String -> Rule a 
test p s = minorRule s f
   where 
      f a
       | p a = [a]
       | otherwise = []  

path :: String -> Rule a 
path s = minorRule s (\a->[a])