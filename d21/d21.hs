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
import Text.Trifecta

type Ingredient = String

type Allergen = String

lineParser :: Parser (Set Ingredient, Set Allergen)
lineParser = (,) <$> ingredientParser <*> (symbol "(contains" *> allergenParser <* symbol ")")
  where
    ingredientParser = Set.fromList <$> (some letter `sepEndBy` space)
    allergenParser = Set.fromList <$> (some letter `sepBy` comma)

mapAllergensToIngredients ::
  Map Allergen (Set Ingredient) ->
  (Set Ingredient, Set Allergen) ->
  Map Allergen (Set Ingredient)
mapAllergensToIngredients accumulator (ingredients, allergens) =
  Set.toList allergens
    |> map (,ingredients)
    |> Map.fromList
    |> Map.unionWith Set.intersection accumulator

p1 :: [(Set Ingredient, Set Allergen)] -> String
p1 linesParsed =
  let unsafeIngredients =
        foldl' mapAllergensToIngredients Map.empty linesParsed |> Map.elems |> Set.unions
   in linesParsed
        |> map (fst .> Set.toList .> flip zip (repeat (1 :: Int)) .> Map.fromList)
        |> Map.unionsWith (+)
        |> flip Map.withoutKeys unsafeIngredients
        |> Map.foldl' (+) 0
        |> show

p2 :: [(Set Ingredient, Set Allergen)] -> String
p2 linesParsed =
  foldl' mapAllergensToIngredients Map.empty linesParsed
    |> Map.assocs
    |> until (productOn' (snd .> Set.size) .> (==) 1) makeExact
    |> sortOn fst
    |> map (snd .> Set.toList .> head)
    |> intercalate ","
  where
    makeExact assocs =
      let (exact, rest) = assocs |> partition (snd .> Set.size .> (==) 1)
          solvedIngredients = map snd exact |> Set.unions
       in exact ++ map (second (`Set.difference` solvedIngredients)) rest

solve :: String -> String
solve =
  lines
    .> traverse (parseString lineParser mempty)
    .> \case
      Failure e -> show e |> error
      Success linesParsed -> [p1 linesParsed, p2 linesParsed] |> unlines

main :: IO ()
main = interact solve
