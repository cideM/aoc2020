#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [extra trifecta containers flow])" -i "runghc -Wall"

import Data.Foldable (toList)
import Data.Maybe (isJust)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Flow
import Text.Trifecta

parse :: Parser [Seq Int]
parse = some (symbol "Player " *> integer *> symbol ":" *> (map fromIntegral .> Seq.fromList <$> some integer))

turn :: (Seq Int, Seq Int) -> (Seq Int, Seq Int)
turn x@(Empty, _) = x
turn x@(_, Empty) = x
turn (a :<| a', b :<| b')
  | a > b = (a' Seq.|> a Seq.|> b, b')
  | b > a = (a', b' Seq.|> b Seq.|> a)
  | otherwise = error "a b equal instructions unclear crab stuck in fan"

winner :: (Seq Int, Seq Int) -> Maybe (Seq Int)
winner (Empty, x) = Just x
winner (x, Empty) = Just x
winner _ = Nothing

score :: Seq Int -> Int
score = toList .> reverse .> flip zip [1 ..] .> fmap (uncurry (*)) .> sum

p2 :: (Seq Int, Seq Int) -> String
p2 = playp2 Set.empty .> either (score .> show) (score .> show)
  where
    playp2 :: Set (Seq Int, Seq Int) -> (Seq Int, Seq Int) -> Either (Seq Int) (Seq Int)
    playp2 _ (Empty, a) = Right a
    playp2 _ (a, Empty) = Left a
    playp2 seen current@(a :<| a', b :<| b')
      | Set.member current seen = fst current |> Left
      | Seq.length a' >= a && Seq.length b' >= b =
        let seen' = Set.insert current seen
         in case playp2 Set.empty (Seq.take a a', Seq.take b b') of
              Left _ -> playp2 seen' (a' Seq.|> a Seq.|> b, b')
              Right _ -> playp2 seen' (a', b' Seq.|> b Seq.|> a)
      | otherwise = turn current |> playp2 (Set.insert current seen)

p1 :: (Seq Int, Seq Int) -> String
p1 = until (winner .> isJust) turn .> winner .> fmap score .> show

solve :: [Seq Int] -> String
solve (a : b : _) = [p1 (a, b), p2 (a, b)] |> unlines
solve _ = error ":("

main :: IO ()
main = interact (parseString parse mempty .> foldResult show solve)
