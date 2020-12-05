#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [])" -i "runghc -Wall"

-- I didn't see that you can just turn the sequence into a binary number for
-- part 1

import Data.Foldable
import Data.List

bsp :: (Char, Int) -> (Char, Int) -> String -> Int
bsp (upperChar, upperInt) (lowerChar, lowerInt) = head . foldl' f [lowerInt .. upperInt]
  where
    f xs c
      | c == upperChar = drop (length xs `div` 2) xs
      | c == lowerChar = take (length xs `div` 2) xs
      | otherwise = error "wrong character"

pass :: String -> Int
pass s =
  let row = bsp ('B', 127) ('F', 0) $ take 7 s
      col = bsp ('R', 7) ('L', 0) . take 3 $ drop 7 s
   in row * 8 + col

findSeat :: [Int] -> [Int]
findSeat xs = [b - 1 | (a, b) <- zip xs (drop 1 xs), b - a == 2]

main :: IO ()
main = do
  result <- lines <$> getContents
  print . (<>) "Part 1: " . show . maximum $ map pass result
  print . (<>) "Part 2: " . show . findSeat . sort $ map pass result
