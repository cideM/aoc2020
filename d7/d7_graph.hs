#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [trifecta parsers extra fgl])" -i "runghc -Wall"

import Control.Applicative
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.NodeMap as GM
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List.Extra (nub, trim, unfoldr)
import Data.Semigroup
import Text.Parser.Token
import Text.Trifecta

type Edge = (String, String, Integer)

type Graph = Gr String Integer

parseLine :: Parser ([String], [Edge])
parseLine = do
  from <- colorP
  tos <- (bagP `sepBy` comma) <* skipMany (satisfy $ (/=) '\n')
  let edges = map (\(label, to) -> (from, to, label)) tos
  return (from : map snd tos, edges)
  where
    words' = letter <|> space
    colorP = trim <$> manyTill words' (symbol "bags contain" <|> symbol "bags contain no other bags")
    bagP = (,) <$> natural <*> (trim <$> manyTill words' (symbol "bags" <|> symbol "bag"))

mkGraph :: [([String], [Edge])] -> (Graph, GM.NodeMap String)
mkGraph xs = GM.mkMapGraph (concatMap fst xs) (concatMap snd xs)

unroll :: (a -> [a]) -> a -> [a]
unroll fn n = concat $ unfoldr f [n]
  where
    f nodes = case concatMap fn nodes of
      [] -> Nothing
      xs -> Just (xs, xs)

p1 :: Graph -> G.Node -> [G.Node]
p1 gr = unroll (G.pre gr)

p2 :: Graph -> (G.Node, Integer) -> [(G.Node, Integer)]
p2 gr = unroll (\(name, label) -> concat . replicate (fromIntegral label) $ G.lsuc gr name)

main :: IO ()
main = do
  result <- parseString (parseLine `sepEndBy` newline) mempty <$> getContents
  case result of
    Failure e -> print e
    Success graphData -> do
      let (graph, nodemap) = mkGraph graphData
          ((node, _), _) = GM.mkNode nodemap "shiny gold"
      print . length . nub $ p1 graph node
      print . foldMap (Sum . snd) $ p2 graph (node, 1)
