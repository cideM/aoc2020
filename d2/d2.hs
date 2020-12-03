#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [])" -i "runghc -Wall"

{-# LANGUAGE LambdaCase #-}

import Text.Read (readEither)

data Line = Line Int Int Char String

readLine :: String -> Either String Line
readLine s =
  let [range, [seek, _], input] = words s
      -- There's no splitOn in the Prelude
      [lower, upper] = words [if c == '-' then ' ' else c | c <- range]
   in do
        l <- readEither lower
        u <- readEither upper
        Right $ Line l u seek input

isValid :: Line -> Bool
isValid (Line lower upper seek input) =
  let count = length $ filter ((==) seek) input
   in count >= lower && count <= upper

isValidPartTwo :: Line -> Bool
isValidPartTwo (Line lower upper seek input) =
  let pos1 = input !! (lower - 1)
      pos2 = input !! (upper - 1)
   in case (pos1 == seek, pos2 == seek) of
        (True, False) -> True
        (False, True) -> True
        _ -> False

main :: IO ()
main =
  sequence . map readLine . lines <$> getContents >>= \case
    Left e -> print e
    Right success -> do
      print $ "Valid passwords part one: " <> show (length $ filter isValid success)
      print $ "Valid passwords part two: " <> show (length $ filter isValidPartTwo success)
