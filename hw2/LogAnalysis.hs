module LogAnalysis where

import Log
import Data.List
import Data.List.Split 

parseStr :: String -> (String, String,String)
parseStr str = let strList = splitOn " " str in
  let messageType = head strList in
  let timeStamp = head (tail strList) in
  let content = intercalate " " (tail (tail strList)) in
  (messageType, timeStamp, content)

parseMessage :: String -> LogMessage
parseMessage messageStr = let strList = splitOn " " messageStr in
  let s1 = head strList in
  let s2 = read $ head (tail strList) in {-- if Error, this part is a int  --}
  let t1 = read $ head (tail strList) in
  let t2 = read $ head (tail (tail strList)) in
  let content1 = intercalate " " (tail (tail strList)) in
  let content2 = intercalate " " (tail (tail (tail strList))) in
      if s1 == "I"
      then LogMessage Info t1 content1
      else if s1 == "W"
           then LogMessage Warning t1 content1
           else LogMessage (Error s2) t2 content2

parse :: String -> [LogMessage]
parse logStr = let strList = splitOn "\n" logStr in
  map parseMessage strList


parsetest :: String -> [String]
parsetest str = splitOn " " str


insertTree :: LogMessage -> MessageTree -> MessageTree
insertTree logMessage messageTree =
  let LogMessage (messageType, _, _) = logMessage in
  if messageType == Info || messageType == Warning
     then messageTree
  else if messageTree == Leaf
       then (Leaf logMessage Leaf)
       else
         let (leftTree, ((_, nodeNum),_, _), rightTree) = messageTree in
         let LogMessage ((_, n), _, _) = logMessage  in
         if n > nodeNum
            then insert logMessage rightTree
         else if n < nodeNum
                 then insert logMessage leftTree
              else messageTree
