module LogAnalysis where

import Log
import Data.List
import Data.List.Split 

parseStr :: String -> (String, String, String)
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
insertTree (Unknown _) messageTree = messageTree
insertTree (LogMessage Info _ _) messageTree = messageTree
insertTree (LogMessage Warning _ _) messageTree = messageTree
insertTree logMessage Leaf = Node Leaf logMessage Leaf
insertTree logMessage messageTree =
  let (LogMessage (Error num) t s)= logMessage in
  let (Node leftTree nodeLogMessage rightTree) = messageTree in
  let (LogMessage (Error nodeNum) _ _) = nodeLogMessage in
  if num > nodeNum then (Node leftTree nodeLogMessage (insertTree logMessage rightTree))
  else if num < nodeNum then (Node (insertTree logMessage leftTree) nodeLogMessage rightTree)
       else messageTree

build :: [LogMessage] -> MessageTree
build l = _build l Leaf
  where _build [] messageTree = messageTree
        _build (x:xs) messageTree = _build xs (insertTree x messageTree)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder messageTree =
  let Node leftTree logMessage rightTree = messageTree in
  let _inOrder (Node leftTree logMessage rightTree) = (inOrder leftTree) ++ [logMessage] ++ (inOrder rightTree) in
  _inOrder messageTree

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong logMessages =
  let orderedLogMessages = inOrder (build logMessages) in
  _filter orderedLogMessages []
  where _filter [] l = l
        _filter (x:xs) l =
          let LogMessage (Error severity) _ str = x in
          if severity >= 50 then _filter xs l++[str]
          else _filter xs l
