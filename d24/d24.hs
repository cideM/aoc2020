#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [extra containers trifecta flow])" -i "runghc -Wall"

{-# LANGUAGE TupleSections #-}

import Data.Foldable (foldl')
import Data.Functor (($>))
import Data.List (scanl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Flow
import Text.Trifecta

newtype Point = Point (Int, Int, Int) -- X Y Z
  deriving (Show, Ord, Eq)

newtype Direction = Direction (Int, Int, Int) -- DX DY DZ
  deriving (Show, Ord, Eq)

data Color = White | Black
  deriving (Show, Eq)

type Grid = Map Point Color

move :: Point -> Direction -> Point
move (Point (x, y, z)) (Direction (dx, dy, dz)) = Point (x + dx, y + dy, z + dz)

buildGrid :: [[Direction]] -> Grid
buildGrid =
  concatMap (scanl' move (Point (0, 0, 0)))
    .> map (,White)
    .> Map.fromList

grow :: Grid -> Grid
grow g =
  Map.keys g
    |> concatMap neighbours
    |> map (,White)
    |> Map.fromList
    |> Map.union g

flipAtPath :: Point -> Grid -> [Direction] -> Grid
flipAtPath p g ds = Map.alter f (foldl' move p ds) g
  where
    f Nothing = Just White
    f (Just Black) = Just White
    f (Just White) = Just Black

directions :: [(Int, Int, Int)]
directions =
  [ (1, -1, 0), -- north east ne
    (1, 0, -1), -- east e
    (0, 1, -1), -- south east se
    (-1, 1, 0), -- south west sw
    (-1, 0, 1), -- west w
    (0, -1, 1) -- north west nw
  ]

neighbours :: Point -> [Point]
neighbours p = map (Direction .> move p) directions

artsyStuff :: Grid -> Grid
artsyStuff g = grow g |> Map.assocs |> map f |> Map.fromList |> flip Map.union g
  where
    f input@(k, v)
      | v == Black && (numBlackTiles == 0 || numBlackTiles > 2) = (k, White)
      | v == White && numBlackTiles == 2 = (k, Black)
      | otherwise = input
      where
        numBlackTiles =
          neighbours k
            |> mapMaybe (`Map.lookup` g)
            |> filter (== Black)
            |> length

parse :: Parser [[Direction]]
parse =
  zip ["ne", "e", "se", "sw", "w", "nw"] directions
    |> map (\(s, vec) -> string s $> Direction vec)
    |> choice
    |> some
    |> flip sepEndBy newline

solve :: [[Direction]] -> String
solve paths = [p1 |> countBlack |> show, p2 |> countBlack |> show] |> unlines
  where
    grid = buildGrid paths
    countBlack = Map.elems .> filter (== Black) .> length
    p1 = foldl' (flipAtPath (Point (0, 0, 0))) grid paths
    p2 = iterate artsyStuff p1 |> flip (!!) 100

main :: IO ()
main = interact (parseString parse mempty .> foldResult show solve)
