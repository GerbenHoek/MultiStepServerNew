--   request:
--   { "service" : diagnosis
--   , "step" : { "x^2 - 1 = 0 , "x^2 = 1 , x = 1 or x = -1}
--   }
--    
--   response:
--   
--   { "feedback": "Let op: invert slope"
--   , "hint" : "gebruik de abc formule"
--   , "equiv" : 1
--   }
--
--   request:
--   { "service" : workedExample
--   , "task" : "x^2" - 1 = 0"
--   }
--    
--   response:
--   
--   { "workedExample": {{"x^2 - 1" = 0, ""} , {"x^2 = 1", ""} , {"x = 1", "x = -1"}}
--   
--   request:
--   { "service" : getTask
--   , "tskNr" : Integer}
--   
--   response:
--   {"givenTask" : "x^2" - 1 = 0"}
-------------------------------------------------------


-- http://localhost/mbt-server.cgi?input={"task":{"x1":1,"x2":2,"x3":5,"y1":8,"y2":10},"answer":42}
-- https://ideastest.science.uu.nl/cgi-bin/mbt-server.cgi?input={"task":{"x1":1,"x2":2,"x3":5,"y1":8,"y2":10},"answer":42}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List
import Data.Maybe
import Utils.Utils (replace)
import Database.SQLite3 (withDatabase, exec)
import qualified Data.Text as T
import Ideas.Text.JSON
import Network.CGI
import Control.Monad.Trans.Class (lift)
import Polynomial.PolyDiagnosis (stringDiagnosis)
import Polynomial.WorkedExample (stringExample)
import Polynomial.PolyTasks

cgiMain :: CGI CGIResult
cgiMain = do
  setHeader "Content-type" "text/json"
  setHeader "Access-Control-Allow-Origin" "*"
  tsks    <- lift loadTasks
  txt     <- fromMaybe (fail "no input") <$> getInput "input"
  json    <- liftCGI (parseJSON txt)
  --liftIO (logDB $ show json)
  service <- liftCGI (evalDecoderJSON (getString "service") json)
  out     <- liftCGI (decodeService tsks json service)
  liftIO (logCom json out) 
  output $ show out

logDB :: String -> IO ()
logDB txt = withDatabase "/workspace/mbt.db" (`exec` query)
 where
  query = T.concat ["INSERT INTO requests (req) VALUES ('", T.pack txt, "');"]

decodeService :: 
   [String] -> JSON -> String -> Either (Error JSON) JSON
decodeService tsks json s = case s of 
   "diagnosis"     -> evalDecoderJSON dt json
   "workedExample" -> evalDecoderJSON dw json
   "getTask"       -> evalDecoderJSON (dg tsks) json
   "hint"          -> evalDecoderJSON dh json
   "video"         -> evalDecoderJSON dv json
   _               -> error "unknown service"

logCom :: JSON -> JSON -> IO()
logCom json out = let
   eval d = either (const "na") id . evalDecoderJSON d
   evals d = either (const "na") show . evalDecoderJSON d
   service = eval (getString "service") 
   date = eval (getString "date") 
   tskNR = evals (getInt "tskNRid") 
   grpNR = evals (getInt "grpNR")
   stndID = eval (getString "stndID") 
   inp = case service json of 
      "diagnosis"     -> evals (reverse <$> getStrings "steps")
      "workedExample" -> eval (getString "task")
      "getTask"       -> evals (getInt "tskNR")
      "hint"          -> eval (getString "hint")
      "video"         -> evals (getInt "video")
      _ -> const "na"    
   columns = concat $ intersperse "," 
                [ "'service'"
                , "'timeStamp'"
                , "'studentID'"
                , "'testGroup'"
                , "'taskNumber'"
                , "'input'"
                , "'output'"]
   values = concat $ intersperse "," $
                        do a <- [ service
                                , date 
                                , stndID
                                , grpNR
                                , tskNR
                                , inp]
                           return $ "'" ++ a json ++ "'"
                     ++ ["'" ++ show out ++ "'"]
   query = if stndID json == "" then T.pack "" 
           else T.pack $ 
                "INSERT INTO requests (" ++ columns ++ ")" ++ 
                "VALUES (" ++ values ++ ")"
   in withDatabase "/workspace/mbt.db" (`exec` query)
   
liftCGI :: Show err => Either err a -> CGI a
liftCGI = either (fail . show) return

getString :: String -> DecoderJSON String
getString s = jObject $ jKey s jString

getStrings :: String -> DecoderJSON [String]
getStrings s = 
   jObject $ jKey s $ jArrayOf jString 

getInt :: String -> DecoderJSON Int
getInt s = jObject $ jKey s jInt

responseT :: (String, String, Integer) -> JSON
responseT (s, h, n) = 
   Object [ ("feedback", String s)
          , ("hint", String h)
          , ("equiv", Integer n)]

responseW :: [[String]] -> JSON
responseW ss = 
   Object [("workedExample", Array [Array (map String s) | s <- ss])]

responseG :: String -> JSON
responseG s =
   Object [("givenTask", String s)]

dt :: DecoderJSON JSON
dt = (responseT . stringDiagnosis) <$> (getStrings "steps")  

dw :: DecoderJSON JSON
dw = (responseW . stringExample) <$> (getString "task")

dg :: [String] -> DecoderJSON JSON
dg as = (responseG . (as !!)) <$> (getInt "tskNR")

dh :: DecoderJSON JSON
dh = responseH <$> (getString "hint")
   where responseH s = Object [("hint", String s)]

dv :: DecoderJSON JSON
dv = responseV <$> (getInt "video")
   where responseV s = Object [("video for task", Integer $ fromIntegral s)]


main :: IO () 
main = runCGI (handleErrors cgiMain)
   --writeParameters "src/Parameters/nillProduct" (nillProductT 750)
   --writeParameters "src/Parameters/twoTerm" (twoTermT 750)
   --writeParameters "src/Parameters/linSquare" (linSquareT 750)
   --writeParameters "src/Parameters/factorThreeTerm" (factorThreeTermT  750)
   --writeParameters "src/Parameters/factorize" (factorizeT 750)

