#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [containers])" -i "runghc -Wall"

import Data.IntMap.Strict ((!))
import qualified Data.IntMap.Strict as IM
import Data.List

p2 :: [Int] -> Int
p2 ns = foldl' f (IM.singleton (head ns) 1) (tail ns) ! maximum ns
  where
    f m v =
      let fwd key = IM.findWithDefault 0 key m
          a = fwd $ v - 1
          b = fwd $ v - 2
          c = fwd $ v - 3
       in IM.insert v (a + b + c) m

main :: IO ()
main = do
  numbers <- map read . lines <$> getContents
  let numbers' = sort $ 0 : (maximum numbers + 3) : numbers
  print . product . take 2 . map length . group . sort . filter (flip elem [1, 3]) . zipWith (flip (-)) numbers' $ tail numbers'
  print $ p2 numbers'
