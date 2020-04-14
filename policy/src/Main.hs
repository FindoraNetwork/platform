module Main where

import PolicyLang
import           System.IO (hGetContents,openFile,IOMode(..),hFlush,stdout,stdin)

-- main = alloyish_main
main = do
  polf <- hGetContents stdin

  compile putStrLn polf

  return ()


