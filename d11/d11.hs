#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [containers extra])" -i "runghc -Wall"

{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (forM_)
import Data.Function ((&))
import Data.List.Extra (groupOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import GHC.List (iterate')

-- | Get direct neighbours in all directions
adjacentp1 :: Map (Int, Int) Char -> (Int, Int) -> [Char]
adjacentp1 m (row, col) =
  catMaybes $
    map
      (flip Map.lookup m)
      [ (row - 1, col - 1),
        (row - 1, col),
        (row - 1, col + 1),
        (row, col - 1),
        (row, col + 1),
        (row + 1, col - 1),
        (row + 1, col),
        (row + 1, col + 1)
      ]

-- | Get the 'line-of-sight' neighbours in every direction
adjacentp2 :: Map (Int, Int) Char -> (Int, Int) -> [Char]
adjacentp2 m pos =
  [ walk (-1, -1),
    walk (-1, 0),
    walk (-1, 1),
    walk (0, -1),
    walk (0, 1),
    walk (1, -1),
    walk (1, 0),
    walk (1, 1)
  ]
  where
    walk dir =
      iterate (f dir) (pos, Nothing)
        & drop 1 -- iterate f x returns the first 'x' as well, which we don't want
        & dropWhile dropF -- Discard the new positions until we reach either 'out of bounds', or an empty or occupied seat
        & head -- The 'dropWhile' above will result in a singleton list, so we extract that one value
        & snd -- Discard position, we only want the value
        & maybe '.' id -- Replace 'out of bounds' with a floor, which is also a 'No Operation' as far as the rules are concerned
    dropF (_, Nothing) = False
    dropF (_, Just '#') = False
    dropF (_, Just 'L') = False
    dropF _ = True
    f (dy, dx) ((row, col), _) =
      let pos' = (row + dy, col + dx)
       in (pos', Map.lookup pos' m)

newField ::
  -- | How many occupied seats it takes to flip an occupied seat
  Int ->
  -- | Function for getting "adjacent" seats where adjacent can mean direct neighbours but also line of sight neighbours
  (Map (Int, Int) Char -> (Int, Int) -> [Char]) ->
  -- | Input map
  Map (Int, Int) Char ->
  -- | Position for which we want to generate a new value
  (Int, Int) ->
  -- | New value for above position
  Char
newField magicNumber getAdjacents m pos =
  let adj = getAdjacents m pos
   in case Map.lookup pos m of
        Nothing -> error "field not in map"
        Just c -> case c of
          '.' -> '.'
          'L' ->
            if not $ '#' `elem` adj
              then '#'
              else 'L'
          '#' ->
            if (<=) magicNumber . length $ filter ('#' ==) adj
              then 'L'
              else '#'
          _ -> error $ "unknown character: " <> show c

-- Utility function for debugging. Prints a map in the same style as the AOC
-- input.
printmap :: Map (Int, Int) Char -> IO ()
printmap m =
  (Map.assocs m :: [((Int, Int), Char)])
    & groupOn row
    & map (map snd)
    & flip forM_ print
  where
    row ((r, _), _) = r

solve ::
  -- | Function for creating a new field
  (Map (Int, Int) Char -> (Int, Int) -> Char) ->
  -- | Input map
  Map (Int, Int) Char ->
  Map (Int, Int) Char
solve mkNewField start = fst . head . dropWhile ((==) False . snd) $ iterate' f (start, False)
  where
    f (m, _) =
      let m' = Map.foldrWithKey' (foldF m) Map.empty m
       in (m', m == m')
    foldF old key _ acc = Map.insert key (mkNewField old key) acc

main :: IO ()
main = do
  ls <- lines <$> getContents
  let m = Map.fromList [((row, col), b) | (row, line) <- zip [0 ..] ls, (col, b) <- zip [0 ..] line]
  print . length . filter ('#' ==) . Map.elems $ solve (newField 4 adjacentp1) m
  print . length . filter ('#' ==) . Map.elems $ solve (newField 5 adjacentp2) m
