#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [containers trifecta flow])" -i "ghc -O2 -Wall"

-- This is the only program that doesn't use runghc. Instead this just compiles
-- with optimizations, so run cat input.txt | ./d23/d23

import Data.Char (digitToInt)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Flow
import Text.Trifecta hiding (lower, upper)

parse :: Parser [Int]
parse = map digitToInt <$> many digit

turn :: Int -> Int -> Int -> IntMap Int -> IntMap Int
turn _ 0 _ m = m
turn upper stepsLeft current m =
  -- For the first iteration, current will be "3". So we get the value at key
  -- "3", which is "8", the next key! That way we follow the connections of our
  -- totally shit linked list :')
  -- 389125467
  let firstPick = m IntMap.! current -- 8
      secondPick = m IntMap.! firstPick -- 9
      thirdPick = m IntMap.! secondPick -- 1
      afterPicks = m IntMap.! thirdPick -- 2
      -- Removing a few nodes just means connecting the start of the segment we want
      -- to remove to a node after the segment.
      m' = IntMap.insert current afterPicks m -- 3 -> 2
      -- Extremely clever trick I found on Reddit! The destination is the
      -- current cup (c) minus one unless...
      -- c - 1 was picked up or...
      -- c - 2 was picked up or...
      -- c - 3 was picked up or...
      -- we're now lower than the lowest number so wrap around but...
      -- max was picked up or...
      -- max - 1 was picked up or...
      -- max - 2 was picked up or...
      -- If after all those things we still can't find a destination something's
      -- wrong. So there's no point in checking a ton of values.
      destination =
        head
          [ x
            | x <-
                [ current -1,
                  current -2,
                  current -3,
                  current -4,
                  upper,
                  upper -1,
                  upper -2
                ],
              x > 0 && x `notElem` [firstPick, secondPick, thirdPick]
          ]
      -- We need to store the next node before we re-assign the destination to some
      -- other node.
      afterDestination = m IntMap.! destination -- 5
      -- It's enough to connect the destination to the first node of the ones we
      -- picked up. The 2nd and 3rd node we picked up can still be accessed through
      -- the link between firstPick and secondPick. We just need to connect
      -- thirdPick to after
      m'' = IntMap.insert destination firstPick m' -- 2 -> 8
      m''' = IntMap.insert thirdPick afterDestination m'' -- 1 -> 5
   in turn upper (stepsLeft - 1) (m''' IntMap.! current) m'''

p1 :: [Int] -> IntMap Int
p1 xs =
  -- Let's create a linked list in the form of a map.
  -- For an input list of 3214 we want to link things like so:
  --
  -- 3 -> 2; 2 -> 1; 1 -> 4; 4 -> 1
  --
  -- So the value of each key is the next key. Creating such a map is what the
  -- weird combination of "tail" and "head" does.
  --
  -- 3214
  -- 2143
  let poorLinkedList = zip xs (tail xs ++ [head xs]) |> IntMap.fromList
      start = head xs
   in -- Clever trick to repeatedly follow the node connections. It's just gonna
      -- run "m IntMap.! 1" first, followed by "m IntMap.! previousResult". If
      -- we start by 1 and take as many as the poor linked list is long we end
      -- up at the node after 1.
      turn (maximum xs) 100 start poorLinkedList

p2 :: [Int] -> IntMap Int
p2 xs =
  let xs' = xs ++ [10 .. 1000000]
      start = head xs'
      poorLinkedList = zip xs' (tail xs' ++ [head xs']) |> IntMap.fromList
   in turn (maximum xs') 10000000 start poorLinkedList

solve :: [Int] -> String
solve xs =
  [ p1 xs |> toList |> show,
    p2 xs |> toList |> take 2 |> product |> show
  ]
    |> unlines
  where
    toList m =
      iterate (m IntMap.!) 1
        |> take (length xs)
        |> drop 1

main :: IO ()
main = interact (parseString parse mempty .> foldResult show solve)
