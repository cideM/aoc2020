#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [])" -i "runghc -Wall"

p1 :: [(Char, Int)] -> Int
p1 = go (0, 0) (1, 0)
  where
    go (x, y) _ [] = abs x + abs y
    go pos@(x, y) vec@(dx, dy) ((c, n) : rest) =
      case c of
        'N' -> go (x, y + n) vec rest
        'E' -> go (x + n, y) vec rest
        'S' -> go (x, y - n) vec rest
        'W' -> go (x - n, y) vec rest
        'F' -> go (x + n * dx, y + n * dy) vec rest
        'L' ->
          let vec' = case n of
                90 -> (- dy, dx)
                180 -> (- dx, - dy)
                270 -> (dy, - dx)
                _ -> error ":("
           in go pos vec' rest
        'R' ->
          let vec' = case n of
                90 -> (dy, - dx)
                180 -> (- dx, - dy)
                270 -> (- dy, dx)
                _ -> error ":("
           in go pos vec' rest
        _ -> error ":("

p2 :: [(Char, Int)] -> Int
p2 = go (0, 0) (10, 1)
  where
    go (x, y) _ [] = abs x + abs y
    go pos@(x, y) vec@(dx, dy) ((c, n) : rest) =
      case c of
        'N' -> go pos (dx, dy + n) rest
        'E' -> go pos (dx + n, dy) rest
        'S' -> go pos (dx, dy - n) rest
        'W' -> go pos (dx - n, dy) rest
        'F' -> go (x + n * dx, y + n * dy) vec rest
        'L' ->
          let vec' = case n of
                90 -> (- dy, dx)
                180 -> (- dx, - dy)
                270 -> (dy, - dx)
                _ -> error ":("
           in go pos vec' rest
        'R' ->
          let vec' = case n of
                90 -> (dy, - dx)
                180 -> (- dx, - dy)
                270 -> (- dy, dx)
                _ -> error ":("
           in go pos vec' rest
        _ -> error ":("

main :: IO ()
main = do
  instructions <- map (\(c : n) -> (c, read n)) . lines <$> getContents
  print $ p1 instructions
  print $ p2 instructions
