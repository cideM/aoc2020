#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [])" -i "runghc -Wall"

makeMap :: String -> [[Int]]
makeMap = map (map convert) . lines
  where
    convert '#' = 1
    convert _ = 0

trees :: [[Int]] -> (Int, Int) -> Int
trees forest (dx, dy) =
  sum [cycle (forest !! y) !! (dx * adjust y) | y <- [0, dy .. length forest - 1]]
  where
    adjust 0 = 0
    adjust n = n `div` dy

main :: IO ()
main = do
  forest <- makeMap <$> getContents
  let results = map (trees forest) [(3, 1), (1, 1), (5, 1), (7, 1), (1, 2)]
  print . (<>) "Part 1: " . show $ product results
  print . (<>) "Part 2: " . show $ head results
