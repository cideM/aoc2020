#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [trifecta parsers containers])" -i "runghc -Wall"

{-# LANGUAGE LambdaCase #-}

import Control.Applicative
import Control.Monad (forM_)
import Data.Either
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Text.Parser.Combinators
import Text.Trifecta

data Height = Cm Integer | In Integer deriving (Show)

data EyeColor = Amb | Blu | Brn | Gry | Grn | Hzl | Oth deriving (Show)

data Passport = Passport Integer Integer Integer Height String EyeColor String deriving (Show)

parseField :: Parser (String, String)
parseField = (,) <$> key <*> value
  where
    key = manyTill (noneOf [' ', '\n']) colon
    value = many (noneOf [' ', '\n'])

fields :: Parser [(String, String)]
fields = parseField `sepEndBy` space

valid :: Map String String -> Maybe Bool
valid m =
  forM_ ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"] (`Map.lookup` m) >> return True

extract :: String -> Parser a -> Map String String -> Either String a
extract key parser m = case Map.lookup key m of
  Nothing -> Left $ key <> " not found"
  Just v -> foldResult (Left . show) Right $ parseString parser mempty v

toPassport :: Map String String -> Either String Passport
toPassport m =
  Passport
    <$> extract "byr" byrP m
    <*> extract "iyr" iyrP m
    <*> extract "eyr" eyrP m
    <*> extract "hgt" heightP m
    <*> extract "hcl" hairP m
    <*> extract "ecl" eyeP m
    <*> extract "pid" idP m
  where
    byrP =
      integer >>= \case
        n
          | n >= 1920 && n <= 2002 -> return n
          | otherwise -> raiseErr $ failed "year not in range"
    iyrP =
      integer >>= \case
        n
          | n >= 2010 && n <= 2020 -> return n
          | otherwise -> raiseErr $ failed "issue year not in range"
    eyrP =
      integer >>= \case
        n
          | n >= 2020 && n <= 2030 -> return n
          | otherwise -> raiseErr $ failed "expiration n not in range"
    heightP :: Parser Height
    heightP =
      let cmP = Cm <$> (integer <* symbol "cm")
          inP = In <$> (integer <* symbol "in")
       in try cmP <|> try inP >>= \case
            v@(Cm h)
              | h >= 150 && h <= 193 -> return v
              | otherwise -> raiseErr $ failed "cm height not in range"
            v@(In h)
              | h >= 59 && h <= 76 -> return v
              | otherwise -> raiseErr $ failed "in height not in range"
    hairP :: Parser String
    hairP = (:) <$> char '#' <*> count 6 (oneOf "abcdef" <|> digit)
    eyeP :: Parser EyeColor
    eyeP =
      (Amb <$ symbol "amb")
        <|> (Blu <$ symbol "blu")
        <|> (Brn <$ symbol "brn")
        <|> (Gry <$ symbol "gry")
        <|> (Grn <$ symbol "grn")
        <|> (Hzl <$ symbol "hzl")
        <|> (Oth <$ symbol "oth")
    idP :: Parser String
    idP = count 9 digit <* eof

main :: IO ()
main = do
  result <- parseString (fields `sepEndBy` newline) mempty <$> getContents
  case result of
    Failure e -> print e
    Success groups -> do
      let ms = map Map.fromList groups
      print . length . filter isJust $ map valid ms
      print . length . rights $ map toPassport ms
