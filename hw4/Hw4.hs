module Hw4 where

import Data.List

{-- exercise 1 --}
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 l = foldl (*) 1 $ map (\x -> x-2) $ filter even l

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n = foldl (+) 0 $ takeWhile even $ iterate (\x -> div x 2) n

data Tree a = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: Ord a => [a] -> Tree a
foldTree [] = Leaf
foldTree l = foldl insertTree Leaf l
  
insertTree :: Ord a => Tree a -> a -> Tree a
insertTree Leaf x = Node 0 Leaf x Leaf
insertTree (Node depth leftTree n rightTree) x =
  if x < n then
    if leftTree == Leaf then Node depth (Node (depth+1) Leaf x Leaf) n rightTree
    else Node depth (insertTree leftTree x) n rightTree
  else if x > n then
         if rightTree == Leaf then Node depth leftTree n (Node (depth+1) Leaf x Leaf)
         else Node depth leftTree n (insertTree rightTree x)
       else Node depth leftTree n rightTree

{- All above is just used for practise -}

{-- AVL Tree --}

getHeight :: Ord a => Tree a -> Integer
getHeight Leaf = -1
getHeight (Node depth _ _ _) = depth

setHeight :: Ord a => Tree a -> Integer -> Tree a
setHeight Leaf _ = Leaf
setHeight (Node _ leftTree val rightTree) newDepth = Node newDepth leftTree val rightTree

rotateLL :: Ord a => Tree a -> Tree a
rotateLL Leaf = Leaf
rotateLL (Node depth Leaf rootVal rightTree) = Node depth Leaf rootVal rightTree
rotateLL (Node depth leftTree rootVal rightTree) =
  let Node leftDepth lL leftVal lR = leftTree in
  let heightRight = maximum [(getHeight lR), (getHeight rightTree)]+1 in
  let heightRoot = maximum [(getHeight lL), heightRight]+1 in
  Node heightRoot lL leftVal (Node heightRight lR rootVal rightTree)

rotateRR :: Ord a => Tree a -> Tree a
rotateRR Leaf = Leaf
rotateRR (Node depth leftTree rootVal Leaf) = Node depth leftTree rootVal Leaf
rotateRR (Node depth leftTree rootVal rightTree) =
  let Node rightDepth rL rightVal rR = rightTree in
  let heightLeft = maximum [(getHeight rL), (getHeight leftTree)]+1 in
  let heightRoot = maximum [(getHeight rR), heightLeft]+1 in
  Node heightRoot (Node heightLeft leftTree rootVal rL) rightVal rR

rotateLR :: Ord a => Tree a -> Tree a
rotateLR Leaf = Leaf
rotateLR (Node depth leftTree rootVal rightTree) =
  rotateLL ( Node depth (rotateRR leftTree) rootVal rightTree)

rotateRL :: Ord a => Tree a -> Tree a
rotateRL Leaf = Leaf
rotateRL (Node depth leftTree rootVal rightTree) =
  rotateRR (Node depth leftTree rootVal (rotateLL rightTree))

insertAVLTree :: Ord a => Tree a -> a -> Tree a
insertAVLTree Leaf x = Node 0 Leaf x Leaf
insertAVLTree (Node depth leftTree n rightTree) x =
  let unchangedTree =
        if x < n then
          let newTree =
                if leftTree == Leaf then Node depth (Node 0 Leaf x Leaf) n rightTree
                else Node depth (insertAVLTree leftTree x) n rightTree in
          let Node _ newLeftTree newVal newRightTree = newTree in
          let balanceFactor = (getHeight newLeftTree) - (getHeight newRightTree) in
          if balanceFactor == 2 then
            let Node  _ _ leftVal _ = newLeftTree in
            if x < leftVal then rotateLL newTree
            else rotateLR newTree
          else newTree
        else if x > n then
               let newTree =
                     if rightTree == Leaf then Node depth leftTree n (Node 0 Leaf x Leaf)
                     else Node depth leftTree n (insertAVLTree rightTree x) in
               let Node _ newLeftTree newVal newRightTree = newTree in
               let balanceFactor = (getHeight newRightTree) - (getHeight newLeftTree) in
               if balanceFactor == 2 then
                 let Node _ _ rightVal _ = newRightTree in
                 if x > rightVal then rotateRR newTree
                 else rotateRL newTree
               else newTree
             else Node depth leftTree n rightTree in
  let Node unDepth unLeftTree unVal unRightTree = unchangedTree in
  setHeight unchangedTree (maximum [(getHeight unLeftTree), (getHeight unRightTree)]+1)

foldAVLTree :: Ord a => [a] -> Tree a
foldAVLTree [] = Leaf
foldAVLTree l = foldl insertAVLTree Leaf l

{-- Exercise 3 --}
xor :: [Bool] -> Bool
xor [] = False
xor l = foldl xor' False l where
  xor' True True = False
  xor' True False = True
  xor' False True = True
  xor' False False = False

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f l = loop l [] where
  loop [] res = res
  loop (x:xs) res = loop xs (res ++ [f x])

{-- Optional --}
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f x [] = x

{-- Exercise 4 --}
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2*x + 1) [ x | x <- [1..n], elem x [i+j+2*i*j | j <- [1..n], i<-[1..j], i+j+2*i*j < n] == False ]

