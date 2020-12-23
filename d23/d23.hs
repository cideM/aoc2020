#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [containers trifecta flow])" -i "ghc -O2 -Wall"

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Char (digitToInt)
import Data.Foldable (toList)
import Data.List (intercalate)
import Data.Ord (Down (..))
import Data.Sequence (Seq (..), (><))
import qualified Data.Sequence as Seq
import Flow
import Text.Trifecta

parse :: Parser (Seq Int)
parse = map digitToInt .> Seq.fromList <$> many digit

p2 :: Int -> Seq Int -> Int
p2 moves game =
  let !max' = maximum game + 1
      !add = [max' .. 1000000] |> Seq.fromList
      game' = game >< add
   in p1 moves game' |> take 2 |> product

p1 :: Int -> Seq Int -> [Int]
p1 !moves !game =
  Seq.iterateN moves turn game
    |> toList
    |> last
    |> final
    |> toList
  where
    final :: Seq Int -> Seq Int
    final !xs =
      let (!before, _ :<| after) = Seq.breakl (1 ==) xs
       in after >< before
    turn (x :<| xs) =
      let (!picked, !xs') = Seq.splitAt 3 xs
          (!smaller, !larger) = Seq.partition (< x) xs'
          (!destination :<| _) = Seq.sortOn Down smaller >< Seq.sortOn Down larger
       in case Seq.findIndexL (destination ==) xs' of
            Nothing -> error ":("
            Just i ->
              let (!before, !after) = Seq.splitAt (i + 1) xs'
               in (before >< picked >< after) Seq.|> x
    turn _ = error "passing empty Seqs around, are we?"

solve :: Seq Int -> String
solve xs = [p1 101 xs |> map show |> intercalate "", p2 1 xs |> show] |> unlines

main :: IO ()
main = interact (parseString parse mempty .> foldResult show solve)
