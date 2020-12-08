#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [trifecta containers parsers])" -i "runghc -Wall"

import Control.Applicative
import Data.Either
import qualified Data.IntSet as IS
import Data.Vector (Vector, (!), (!?), (//))
import qualified Data.Vector as V
import Text.Parser.Token
import Text.Trifecta

data Instruction = Nop Integer | Acc Integer | Jmp Integer deriving (Show)

parseLine :: Parser Instruction
parseLine = insP "nop" Nop <|> insP "acc" Acc <|> insP "jmp" Jmp
  where
    insP s c = symbol s >> c <$> integer'

p1 ::
  Integer -> -- Initial accumulator
  Vector Instruction -> -- Instructions
  Either Integer Integer -- Left means cycle, Right means terminated, always returns accumulator
p1 initialAcc allIns = go IS.empty initialAcc 0
  where
    go seen acc curIdx =
      if IS.member curIdx seen
        then Left acc
        else
          let newSeen = IS.insert curIdx seen
           in case allIns !? curIdx of
                Nothing -> Right acc
                Just (Nop _) -> go newSeen acc (curIdx + 1)
                Just (Acc i) -> go newSeen (acc + i) (curIdx + 1)
                Just (Jmp i) -> go newSeen acc (curIdx + fromIntegral i)

p2 :: Vector Instruction -> Either Integer Integer
p2 allIns =
  -- This is ugly but whatever. Loop over instructions and on each iteration
  -- switch the current one. 
  head [result | i <- [0 .. V.length allIns - 1], result <- [p1 0 (allIns // [(i, switch $ allIns ! i)])], isRight result]
  where
    switch (Nop n) = Jmp n
    switch (Acc n) = Acc n
    switch (Jmp n) = Nop n

main :: IO ()
main = do
  result <- parseString (parseLine `sepEndBy` newline) mempty <$> getContents
  case result of
    Failure e -> print e
    Success ms -> do
      let v = V.fromList ms
      print $ p1 0 v
      print $ p2 v
