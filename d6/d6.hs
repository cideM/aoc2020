#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [extra containers])" -i "runghc -Wall"

import Data.List.Extra (groupOn)
import qualified Data.Set as Set

main :: IO ()
main = do
  ls <- lines <$> getContents
  let gs = map (map Set.fromList) $ groupOn ("" ==) ls
  print . sum $ map (Set.size . Set.unions) gs
  print . sum $ map (Set.size . foldr1 Set.intersection) gs
