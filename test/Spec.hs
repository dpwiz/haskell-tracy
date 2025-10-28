module Main where

import System.Tracy.Bindings
import Foreign.Marshal.Alloc
import Control.Exception

main :: IO ()
main = withProfiler do
  setThreadName "Haskell main"
  connected <- isConnected
  print connected
  srcLoc <- allocSrcLoc 0 "test/Spec.hs" "main" 0xFFFFFFFF
  print srcLoc
  withZone srcLoc \ctx -> do
    message "hello from Haskell!"
    zoneText ctx "some text"
    zoneName ctx "a zone"
    messageL "a literal"

  withZoneSRCLOC 0 "test/Spec.hs" "main" "my zone" 0xFF00FF \_ -> do
    pure ()

  frameMark

  bracket (mallocBytes 1024) free \ptr -> do
    memoryAlloc ptr 1024
    memoryFree ptr

  plotData "my plot" 3.14
