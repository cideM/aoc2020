{-# LANGUAGE ScopedTypeVariables #-}

import Data.Foldable (find)

subsequencesOfSize :: Int -> [a] -> [[a]]
subsequencesOfSize n xs =
  let l = length xs
   in if n > l
        then []
        else subsequencesBySize xs !! (l - n)
  where
    subsequencesBySize [] = [[[]]]
    subsequencesBySize (x : xs) =
      let next = subsequencesBySize xs
       in zipWith
            (++)
            ([] : next)
            (map (map (x :)) next ++ [[]])

main :: IO ()
main = do
  (numbers :: [Int]) <- map read . lines <$> getContents
  let p1 = subsequencesOfSize 2 numbers
      p2 = subsequencesOfSize 3 numbers
  solve p1
  solve p2
  where
    solve = print . fmap product . find ((==) 2020 . sum)
