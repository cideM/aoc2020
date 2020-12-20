#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [trifecta containers])" -i "runghc -Wall"

-- The worst day by far. I feel so stupid. I hate this. All of it. My brain
-- sucks, useless piece of trash.

import Control.Applicative ((<|>))
import Data.Function ((&))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Text.Trifecta
  ( Parser,
    Result (..),
    choice,
    eof,
    integer,
    letter,
    natural,
    parseString,
    some,
    surroundedBy,
    symbol,
    try,
  )

data Rule a = Leaf Char | List [a] | Choice ([a], [a])
  deriving (Show)

newtype RuleRef = RuleRef Int
  deriving (Show)

makeParser :: RuleRef -> IntMap (Rule RuleRef) -> Parser String
makeParser (RuleRef n) m =
  case IM.lookup n m of
    Nothing -> error $ "rule " <> show n <> " not found"
    Just r -> case r of
      Leaf c -> symbol [c]
      List refs -> listToParser refs
      Choice (refs, refs') -> try (listToParser refs) <|> listToParser refs'
  where
    listToParser = fmap concat . traverse (flip makeParser m)

ruleParser :: Parser (Int, Rule RuleRef)
ruleParser = do
  n <- fromIntegral <$> integer
  _ <- symbol ":"
  rest <-
    choice
      [ (try alternativeP),
        (List . map (RuleRef . fromIntegral) <$> some natural),
        (Leaf <$> letter `surroundedBy` symbol "\"")
      ]
  return (n, rest)
  where
    alternativeP = do
      n1 <- map (RuleRef . fromIntegral) <$> some natural
      _ <- symbol "|"
      n2 <- map (RuleRef . fromIntegral) <$> some natural
      return $ Choice (n1, n2)

parse :: [String] -> Either String (IntMap (Rule RuleRef))
parse ss = case traverse (parseString ruleParser mempty) ss of
  Failure e -> Left $ show e
  Success v -> Right $ IM.fromList v

solve :: String -> String
solve s =
  let (rules, input) = lines s & break (== "")
   in case parse rules of
        Left e -> e
        Right m ->
          let p42 = makeParser (RuleRef 42) m
              p31 = makeParser (RuleRef 31) m
              p0 = makeParser (RuleRef 0) m <* eof
              p = do
                r42 <- some $ try p42
                r31 <- some p31
                _ <- eof
                if length r42 > length r31 then return r42 else fail ""
              p2 = show . length . filter isSuccess $ map (parseString p mempty) input
              p1 = show . length . filter isSuccess $ map (parseString p0 mempty) input
           in p1 <> " " <> p2
  where
    isSuccess (Success _) = True
    isSuccess _ = False

main :: IO ()
main = interact solve
