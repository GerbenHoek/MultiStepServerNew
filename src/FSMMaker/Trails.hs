module FSMMaker.Trails 
   ( (.*.)
   , (<|>)
   , firsts
   , empty
   , Trail (..)) where

data Trail a = Trail a :*: Trail a
           | Trail a :|: Trail a
           | Many (Trail a)
           | R a
           | Lbl String (Trail a)
           | Succeed
           | Fail
 deriving (Read, Show)
  
-- constructs a sequence like :*:, but maintains invariants for datatype
(.*.) :: Trail a -> Trail a -> Trail a
Succeed .*. b  = b
a .*. Succeed  = a
(a :*: b) .*. c = a .*. (b .*. c)
a .*. b  = a :*: b
infixl 7 .*.


(<|>) :: Trail a -> Trail a -> Trail a
(a :|: b) <|> c = a <|> (b <|> c)
a <|> b = a :|: b
infixl 6 <|> 

-- parsing a trail 
empty :: Trail a -> Bool
empty Succeed    = True
empty Fail       = False
empty (R _)      = False
empty (s:|:t)    = empty s || empty t
empty (s:*:t)    = empty s && empty t
empty (Many _)   = True
empty (Lbl _ t) = empty t 

firsts :: Trail a -> [(a, Trail a)]
firsts Succeed    = []
firsts Fail       = []
firsts (R r)      = [(r, Succeed)]
firsts (s:|:t)    = firsts s ++ firsts t
firsts (s:*:t)    = [(r, s'.*.t) | (r, s') <- firsts s] ++ 
                       if empty s then firsts t else []             
firsts (Many s)   = 
   [(r, s' .*. Many s) | (r, s') <- firsts s] 
firsts (Lbl _ t) = firsts t 

instance Functor Trail where
   fmap _ Succeed   = Succeed
   fmap _ Fail      = Fail
   fmap g (R a)     = R (g a)
   fmap g (a:|:b)   = fmap g a <|> fmap g b 
   fmap g (a:*:b)   = fmap g a .*. fmap g b
   fmap g (Many a)  = Many (fmap g a) 
   fmap g (Lbl s a) = Lbl s (fmap g a) 