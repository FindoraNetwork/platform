module Main where

import PolicyLang
import           System.IO (hGetContents,stdin)

-- main = alloyish_main
main :: IO ()
main = do
  polf <- hGetContents stdin

  (Right _) <- compile putStrLn polf

  return ()


