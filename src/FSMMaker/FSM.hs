module FSMMaker.FSM 
   ( FSM
   , fsm 
   , transitions
   , start
   , accept
   , firsts) where 
import qualified Data.IntMap as IM 
import FSMMaker.Trails (Trail (..))
import Utils.Utils (setNub)
import Control.Monad.State 

data FSM a = FSM { transitions :: IM.IntMap [(Maybe a, Int)]
                 , start       ::  Int 
                 , accept      :: [Int]}                
   deriving Show

rename :: Int -> Int -> FSM a -> FSM a 
rename new old (FSM t s a) = FSM t' s' a'
   where rename' x = if x == old then new else x
         f as = [(ma, rename' y) | (ma, y) <- as] 
         t' = IM.map f $ 
              IM.mapKeys rename' t
         s' = rename' s
         a' = setNub $ map rename' a 

fromTrail :: Trail a -> State Int (FSM a)
fromTrail (R a) = do
   n <- get 
   put (n+2)
   return $ FSM (IM.singleton n [(Just a, (n+1))]) n [n+1]
fromTrail Succeed = do 
   n <- get 
   put (n+2)
   return $ FSM (IM.singleton n [(Nothing, (n+1))]) n [n+1]
fromTrail Fail = do 
   n <- get 
   put (n+1)  
   return $ FSM (IM.singleton n []) n []
fromTrail (a :|: b) = do 
   ma <- fromTrail a
   mb <- fromTrail b
   let mb' = rename (start ma) (start mb) mb
   return $ FSM (IM.unionWith (++) (transitions ma) (transitions mb'))
                (start ma) 
                (setNub $ accept ma ++ accept mb') 
fromTrail (a :*: b) = do 
   ma <- fromTrail a
   mb <- fromTrail b
   let ma' = foldr (rename (start mb)) ma (accept ma) 
   return $ FSM (IM.unionWith (++) (transitions ma') (transitions mb))
                (start ma') 
                (accept mb)
fromTrail (Many a) = do 
   ma <- fromTrail a 
   return $ foldr (rename (start ma)) ma (accept ma)
fromTrail (Lbl _ a) = fromTrail a

fsm :: Trail a -> FSM a
fsm t = evalState (fromTrail (t :*: Succeed)) 0

firsts ::  FSM a -> Int -> [(Maybe a, Int)]
firsts m s = IM.findWithDefault [] s (transitions m)