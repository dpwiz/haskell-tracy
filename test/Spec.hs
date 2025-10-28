module Main where

import qualified System.Tracy.Bindings as Tracy
import Foreign.Marshal.Alloc
import Control.Exception

main :: IO ()
main = Tracy.withProfiler do
  Tracy.setThreadName "Haskell main"
  connected <- Tracy.waitConnected 200000 (Just 5000000)
  print connected
  srcLoc <- Tracy.allocSrcLoc 0 "test/Spec.hs" "main" 0xFFFFFFFF
  print srcLoc
  Tracy.withZone srcLoc \ctx -> do
    Tracy.message "hello from Haskell!"
    Tracy.zoneText ctx "some text"
    Tracy.zoneName ctx "a zone"
    Tracy.messageL "a literal"

  Tracy.withZoneSRCLOC 0 "test/Spec.hs" "main" "my zone" 0xFF00FF \_ -> do
    pure ()

  Tracy.frameMark

  bracket (mallocBytes 1024) free \ptr -> do
    Tracy.memoryAlloc ptr 1024
    Tracy.memoryFree ptr

  Tracy.plotData "my plot" 3.14
