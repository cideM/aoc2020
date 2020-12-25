#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [trifecta flow])" -i "runghc -Wall"

{-# LANGUAGE BangPatterns #-}

import Flow
import Text.Trifecta

transform :: Int -> (Int, Int) -> (Int, Int)
transform subject (!i, !n) = (i + 1, (n * subject) `mod` 20201227)

parse :: Parser (Int, Int)
parse = (,) <$> (fromIntegral <$> integer) <*> (fromIntegral <$> integer)

solve :: (Int, Int) -> String
solve (pubKeyA, pubKeyB) =
  let loop1 = until (snd .> (==) pubKeyA) (transform 7) (0, 1) |> fst
   in until (fst .> (==) loop1) (transform pubKeyB) (0, 1)
        |> snd
        |> show
        |> flip (:) []
        |> unlines

main :: IO ()
main = interact (parseString parse mempty .> foldResult show solve)
