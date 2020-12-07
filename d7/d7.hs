#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [trifecta containers parsers extra])" -i "runghc -Wall"

import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.List.Extra (trim, unfoldr)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Semigroup
import Text.Parser.Token
import Text.Trifecta

type Bag = (Integer, String)

type Recipes = Map String [Bag]

parseLine :: Parser Recipes
parseLine = Map.singleton <$> colorP <*> (bagP `sepBy` comma) <* skipMany (satisfy $ (/=) '\n')
  where
    words' = letter <|> space
    colorP = trim <$> manyTill words' (symbol "bags contain" <|> symbol "bags contain no other bags")
    bagP = (,) <$> natural <*> (trim <$> manyTill words' (symbol "bags" <|> symbol "bag"))

hasColor :: String -> Recipes -> String -> Maybe Bag
hasColor seek m key = do
  children <- Map.lookup key m
  guard . not $ null children
  find ((==) seek . snd) children <|> listToMaybe (mapMaybe (hasColor seek m . snd) children)

p1 :: String -> Recipes -> [Bag]
p1 seek m = mapMaybe (hasColor seek m) $ Map.keys m

p2 :: String -> Recipes -> [Bag]
p2 k m = concat $ unfoldr f [k]
  where
    f keys = case concatMap (\n -> Map.findWithDefault [] n m) keys of
      [] -> Nothing
      xs -> Just (xs, concat [replicate (fromIntegral quantity) name | (quantity, name) <- xs])

main :: IO ()
main = do
  result <- parseString (parseLine `sepEndBy` newline) mempty <$> getContents
  case result of
    Failure e -> print e
    Success ms -> do
      let m = Map.unions ms
      print . length $ p1 "shiny gold" m
      print . getSum . foldMap (Sum . fst) $ p2 "shiny gold" m
