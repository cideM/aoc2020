{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

main :: IO ()
main = do
  (contents :: [Int]) <- map undefined . lines <$> getContents
  print "Hello"
