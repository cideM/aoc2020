#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [trifecta parsers])" -i "runghc -Wall"

import Control.Applicative ((<|>))
import Control.Arrow ((>>>))
import Text.Parser.Expression
import Text.Trifecta

expr :: (Monad m, TokenParsing m) => [[Operator m Integer]] -> m Integer
expr table = buildExpressionParser table (term table) <?> "expression"

term :: (Monad m, TokenParsing m) => [[Operator m Integer]] -> m Integer
term table = parens (expr table) <|> natural <?> "simple expression"

binary :: String -> (a -> a -> a) -> Assoc -> Operator Parser a
binary name fun = Infix (fun <$ symbol name)

main :: IO ()
main = interact showSolution
  where
    tableP1 = [[binary "+" (+) AssocLeft, binary "*" (*) AssocLeft]]
    tableP2 = [[binary "+" (+) AssocLeft], [binary "*" (*) AssocLeft]]
    solve table = lines >>> traverse (parseString (expr table) mempty) >>> fmap sum >>> show
    showSolution s = show $ "p1: " <> solve tableP1 s <> "p2: " <> solve tableP2 s
