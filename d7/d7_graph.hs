#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [trifecta parsers extra fgl])" -i "runghc -Wall"

import Control.Applicative
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.NodeMap as GM
import Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.Graph.Inductive.Query.BFS as BFS
import Data.List.Extra (sumOn', trim, unfoldr)
import Text.Parser.Token
import Text.Trifecta

parseLine ::
  Parser
    ( [String], -- Labels for nodes
      [(String, String, Integer)] -- Edges (from, to, label)
    )
parseLine = do
  from <- colorP
  tos <- (bagP `sepBy` comma) <* skipMany (satisfy $ (/=) '\n')
  let edges = map (\(label, to) -> (from, to, label)) tos
  return (from : map snd tos, edges)
  where
    words' = letter <|> space
    colorP = trim <$> manyTill words' (symbol "bags contain" <|> symbol "bags contain no other bags")
    bagP = (,) <$> natural <*> (trim <$> manyTill words' (symbol "bags" <|> symbol "bag"))

p2 :: Gr String Integer -> (G.Node, Integer) -> [(G.Node, Integer)]
p2 gr n = concat $ unfoldr f [n]
  where
    fn (name, label) = concat . replicate (fromIntegral label) $ G.lsuc gr name
    f nodes = case concatMap fn nodes of
      [] -> Nothing
      xs -> Just (xs, xs)

main :: IO ()
main = do
  result <- parseString (parseLine `sepEndBy` newline) mempty <$> getContents
  case result of
    Failure e -> print e
    Success graphData -> do
      let (graph, nodemap) = GM.mkMapGraph (concatMap fst graphData) (concatMap snd graphData)
          ((node, _), _) = GM.mkNode nodemap "shiny gold"
      print . length . filter (\n -> not . null $ BFS.esp n node graph) . filter (node /=) $ G.nodes graph
      print . sumOn' snd $ p2 graph (node, 1)
