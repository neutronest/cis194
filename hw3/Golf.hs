module Golf where

selectItem :: Int -> [a] -> [a]
selectItem _ [] = []
selectItem n l = loop n 1 l []
  where loop _ _ [] res = res
        loop n cur (x:xs) res = if n == cur then loop n 1 xs (res++[x])
                                else loop n (cur+1) xs res

skips :: [a] -> [[a]]
skips l = let len = length l in
  map loop [1..len]
  where loop n = selectItem n l

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs) = loop (x:y:z:xs) []
  where loop [] res = []
        loop [x] res = res
        loop [x,y] res = res
        loop (x:y:z:xs) res = if y > x && y > z then loop (y:z:xs) (res++[y])
                              else loop (y:z:xs) res

