module Utils (
  groupIntoN
) where

import Data.List ( genericSplitAt )


groupIntoN :: Int -> [a] -> [[a]]
groupIntoN _ [] = []
groupIntoN n xs
  | n > 0 = head_group : groupIntoN n rest
  | otherwise = error "expected n > 0"
  where (head_group, rest) = genericSplitAt n xs

-- intersperseEvery :: Natural -> a -> [a] -> [a]
-- intersperseEvery = 


extrasperse :: a -> [a] -> [a]
extrasperse sep xs = sep : appendEach sep xs

appendEach :: a -> [a] -> [a]
appendEach _ [] = []
appendEach sep (x:xs) = x : sep : appendEach sep xs