module Main where

import LogAnalysis

main :: IO()
--main = let strList = parse1 "123 321" in
--  mapM_ putStrLn strList 
main = let (a, b, c) = parseStr "I 2015:39 this is an apple" in
  do
    putStrLn a
    putStrLn b
    putStrLn c
