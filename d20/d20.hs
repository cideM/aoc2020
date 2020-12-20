#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [extra containers flow])" -i "runghc -Wall"

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad (msum)
import Data.Bifunctor (second)
import Data.List (foldl1', sortOn)
import Data.List.Extra (groupOn, splitOn, transpose)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Flow

data Match = T | B | L | R deriving (Show)

type Grid = [Row]

type Row = [Char]

type Tile = (Int, Grid)

type Pos = (Int, Int)

makeTile :: [String] -> Tile
makeTile (id' : rows) =
  let [_, rest] = words id'
   in (init rest |> read, rows)
makeTile _ = error "can't make tile from crap"

gridToMap :: Grid -> Map Pos Char
gridToMap grid =
  M.fromList
    [ ((row, col), value)
      | (row, content) <- zip [0 ..] grid,
        (col, value) <- zip [0 ..] content
    ]

findMonstersInGrid :: Grid -> Maybe (Map Pos Char, [[Pos]])
findMonstersInGrid = msum . map check . variations
  where
    check grid =
      let m = gridToMap grid
       in findMonsters m |> \case
            [] -> Nothing
            monsters -> Just (m, monsters)

findMonsters :: Map Pos Char -> [[Pos]]
findMonsters m = M.keys m |> mapMaybe (`isMonster` m)

isMonster :: Pos -> Map Pos Char -> Maybe [Pos]
isMonster (x, y) m =
  let possibleSpots =
        [ (x + 18, y),
          (x, y + 1),
          (x + 5, y + 1),
          (x + 6, y + 1),
          (x + 11, y + 1),
          (x + 12, y + 1),
          (x + 17, y + 1),
          (x + 18, y + 1),
          (x + 19, y + 1),
          (x + 1, y + 2),
          (x + 4, y + 2),
          (x + 7, y + 2),
          (x + 10, y + 2),
          (x + 13, y + 2),
          (x + 16, y + 2)
        ]
   in mapM_ check possibleSpots >> return possibleSpots
  where
    check pos = M.lookup pos m >>= \c -> if c == '#' then Just c else Nothing

solve :: String -> String
solve =
  lines
    .> splitOn [""]
    .> map makeTile
    .> puzzle
    .> pure
    .> (<*>) [p1, p2]
    .> unlines
  where
    p2 =
      mergeTiles .> findMonstersInGrid .> \case
        Nothing -> error "No monsters :("
        Just (m', positions) ->
          M.assocs m'
            |> filter (fst .> flip Set.notMember (concat positions |> Set.fromList))
            |> filter (snd .> (==) '#')
            |> length
            |> show
    p1 m =
      let ks = M.keys m
          xs = ks |> map fst
          ys = ks |> map snd
          (minX, maxX) = (minimum xs, maximum xs)
          (minY, maxY) = (minimum ys, maximum ys)
       in [(minX, maxY), (maxX, maxY), (minX, minY), (maxX, minY)]
            |> mapMaybe (`M.lookup` m)
            |> map fst
            |> product
            |> show

crop :: Tile -> Tile
crop = second (tail .> init .> transpose .> tail .> init .> transpose)

mergeTiles :: Map Pos Tile -> Grid
mergeTiles =
  M.assocs
    .> map (second crop)
    .> sortOn (snd . fst)
    .> groupOn (snd . fst)
    .> map (map (snd . snd))
    .> map (foldl1' (zipWith (++)))
    .> reverse
    .> concat

flip', rotate :: Grid -> Grid
rotate = transpose .> map reverse
flip' = map reverse

-- Lazy list of all flip and rotate variations
variations :: Grid -> [Grid]
variations matrix =
  [ iterate flip' (iterate rotate matrix !! x) !! y
    | x <- [0, 1, 2, 3],
      y <- [0, 1]
  ]

top, bottom, left, right :: Grid -> Row
top = head
bottom = last
left = transpose .> head
right = transpose .> last

-- Direction, as seen from a, in which b fits
match :: Grid -> Grid -> Maybe Match
match a b
  | left a == right b = Just L
  | right a == left b = Just R
  | top a == bottom b = Just T
  | bottom a == top b = Just B
  | otherwise = Nothing

matchAny :: Grid -> Grid -> Maybe (Grid, Match)
matchAny a b =
  msum $ combine <$> variations b
  where
    combine b' = (b',) <$> match a b'

puzzle :: [Tile] -> Map Pos Tile
puzzle [] = M.empty
puzzle [a] = M.singleton (0, 0) a
puzzle (a : as) = go (M.singleton (0, 0) a) as
  where
    applyDir L (x, y) = (x -1, y)
    applyDir R (x, y) = (x + 1, y)
    applyDir T (x, y) = (x, y + 1)
    applyDir B (x, y) = (x, y -1)
    combine ::
      -- | Existing position and tile in our growing puzzle
      (Pos, Tile) ->
      -- | Incoming tile, maybe it fits or maybe it doesn't
      Tile ->
      -- | New position of tile in puzzle. The grid inside the tile might be
      -- different from the original, since we rotate and flip it!
      Maybe (Pos, Tile)
    combine (pos, (_, grid)) (id', grid') = do
      (newGrid, direction) <- matchAny grid grid'
      return (applyDir direction pos, (id', newGrid))
    go m [] = m
    go m rest =
      case msum $ combine <$> M.assocs m <*> rest of
        Nothing -> error "no match"
        Just (newPos, newEntry@(newId, _)) ->
          case M.lookup newPos m of
            Just v -> error $ "collision " <> show v <> " " <> show newEntry
            Nothing ->
              -- Recurse with a new tile added to the puzzle and that tile
              -- removed from the rest. It's important to do the comparison on
              -- ID because the grid might have changed!
              go (M.insert newPos newEntry m) $ [x | x@(id', _) <- rest, id' /= newId]

main :: IO ()
main = interact solve
