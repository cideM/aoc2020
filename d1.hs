{-# LANGUAGE ScopedTypeVariables #-}

import Data.Foldable (find)

-- My first attempt was using permutations of length 2 and 3 rather than list
-- comprehensions, but the 'permutations' function in Data.List was too slow.
-- You can find it at 'git show de8663455a75656a889d2094fedbaea5e66aa98f'

main :: IO ()
main = do
  numbers <- map read . lines <$> getContents
  print . take 1 $ [ x * y | x <- numbers, y <- numbers, x + y == 2020 ]
  print . take 1 $ [ x * y * z | x <- numbers, y <- numbers, z <- numbers, x + y + z == 2020 ]
