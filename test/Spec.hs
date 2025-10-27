module Main where

import System.Tracy.Bindings

main :: IO ()
main = do
  srcLoc <- allocSrcLoc 0 "test/Spec.hs" "main" 0xFFFFFFFF
  print srcLoc
  -- TODO: start zone
  -- TODO: emit message
  -- TODO: end zone
