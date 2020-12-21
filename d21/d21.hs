#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [extra trifecta containers flow])" -i "runghc -Wall"

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

import Data.Bifunctor (second)
import Data.List (foldl', intercalate, partition, sortOn)
import Data.List.Extra (productOn')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Flow
import qualified Text.Trifecta as Tri

type Ingredient = String

type Allergen = String

lineParser :: Tri.Parser (Set Ingredient, Set Allergen)
lineParser = do
  ingredients <- Set.fromList <$> Tri.some Tri.letter `Tri.sepEndBy` Tri.space
  _ <- Tri.symbol "(contains"
  allergens <- Set.fromList <$> Tri.some Tri.letter `Tri.sepBy` Tri.comma
  _ <- Tri.symbol ")"
  return (ingredients, allergens)

mapAllergensToIngredients ::
  Map Allergen (Set Ingredient) ->
  (Set Ingredient, Set Allergen) ->
  Map Allergen (Set Ingredient)
mapAllergensToIngredients accumulator (ingredients, allergens) =
  Set.toList allergens
    |> map (,ingredients)
    |> Map.fromList
    |> Map.unionWith Set.intersection accumulator

p1 :: Set Ingredient -> Map Ingredient Int -> String
p1 unsafeIngredients =
  Map.assocs
    .> filter (fst .> flip Set.notMember unsafeIngredients)
    .> map snd
    .> sum
    .> show

p2 :: Map Allergen (Set Ingredient) -> String
p2 =
  Map.assocs
    .> sortOn (snd .> length)
    .> until (productOn' (snd .> Set.size) .> (==) 1) makeExact
    .> sortOn fst
    .> map (snd .> Set.toList .> head)
    .> intercalate ","
  where
    makeExact :: [(Allergen, Set Ingredient)] -> [(Allergen, Set Ingredient)]
    makeExact assocs =
      let (exact, rest) = assocs |> partition (snd .> Set.size .> (==) 1)
          solvedIngredients = exact |> map snd |> Set.unions
          rest' = rest |> map (second (`Set.difference` solvedIngredients))
       in exact ++ rest'

solve :: String -> String
solve =
  lines
    .> traverse (Tri.parseString lineParser mempty)
    .> \case
      Tri.Failure e -> show e |> error
      Tri.Success linesParsed ->
        let occurences =
              linesParsed
                |> map (fst .> Set.toList .> flip zip (repeat 1) .> Map.fromList)
                |> Map.unionsWith (+)
            allergenToIngredients = foldl' mapAllergensToIngredients Map.empty linesParsed
            unsafeIngredients = Map.elems allergenToIngredients |> Set.unions
         in [p1 unsafeIngredients occurences, p2 allergenToIngredients] |> unlines

main :: IO ()
main = interact solve
