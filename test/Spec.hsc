{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
module Spec where

import qualified System.Tracy.Bindings as Tracy
import Control.Concurrent (threadDelay)

#include <tracy/TracyC.h>
import Control.Monad (replicateM_, when)
import Data.IORef
import System.Random (randomIO)

main :: IO ()
main = Tracy.withProfiler do
  Tracy.setThreadName "Haskell main"
  connected <- Tracy.waitConnected 200000 (Just 5000000)
  print connected
  frameCounter <- newIORef 0
  piState <- newIORef (0, 0)
  replicateM_ 60 (runFrame frameCounter piState)
  Tracy.message "Simulation finished"

factorial :: Integer -> Integer
factorial n = product [1..n]

runFrame :: IORef Int -> IORef (Int, Int) -> IO ()
runFrame frameCounter piState = Tracy.withZoneSRCLOC __LINE__ __FILE__ "runFrame" "runFrame" 0x00FF00 $ \_ -> do
  update frameCounter piState
  rendering
  threadDelay 4000
  Tracy.frameMark

update :: IORef Int -> IORef (Int, Int) -> IO ()
update frameCounter piState = Tracy.withZoneSRCLOC __LINE__ __FILE__ "update" "update" 0xFF0000 $ \_ -> do
  i <- atomicModifyIORef' frameCounter (\i -> (i+1, i))
  when (i `mod` 15 == 0) $
    Tracy.withZoneSRCLOC __LINE__ __FILE__ "factorial" "factorial" 0xFF00FF $ \_ -> do
      let !_ = factorial 50000
      pure ()

  estimatePi piState
  physics
  threadDelay 2000

estimatePi :: IORef (Int, Int) -> IO ()
estimatePi piState = Tracy.withZoneSRCLOC __LINE__ __FILE__ "estimatePi" "estimatePi" 0x00FFFF $ \_ -> do
  replicateM_ 1000 $ do
    x <- randomIO :: IO Double
    y <- randomIO :: IO Double
    atomicModifyIORef' piState (\(inside, total) ->
      let newTotal = total + 1
          newInside = if x*x + y*y <= 1 then inside + 1 else inside
      in ((newInside, newTotal), ()))
  (inside, total) <- readIORef piState
  Tracy.plotData "pi" (4 * fromIntegral inside / fromIntegral total)

physics :: IO ()
physics = Tracy.withZoneSRCLOC __LINE__ __FILE__ "physics" "physics" 0x0000FF $ \_ -> do
  threadDelay 5000

rendering :: IO ()
rendering = Tracy.withZoneSRCLOC __LINE__ __FILE__ "rendering" "rendering" 0xFFFF00 $ \_ -> do
  threadDelay 5000
