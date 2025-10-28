module Main where

import System.Tracy.Bindings

main :: IO ()
main = withProfiler do
  connected <- isConnected
  print connected
  srcLoc <- allocSrcLoc 0 "test/Spec.hs" "main" 0xFFFFFFFF
  print srcLoc
  withZone srcLoc \ctx -> do
    message "hello from Haskell!"
    zoneText ctx "some text"
    zoneName ctx "a zone"
    messageL "a literal"
