{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}

module System.Tracy.Bindings (
  withProfiler,
  allocSrcLoc,
  withZone,
  withZoneSRCLOC,
  message,
  messageL,
  zoneText,
  zoneName,
  isConnected,
  setThreadName,
  frameMark,
  memoryAlloc,
  memoryFree,
  plotData,
  waitConnected,
  TracyCZoneCtx(..),
) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Data.Maybe (fromMaybe)
import Foreign.Marshal.Utils (fromBool)
import Data.Word
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.Timeout (timeout)
import Foreign (with)

#define TRACY_ENABLE
#include <tracy/TracyC.h>

data SourceLocationData = SourceLocationData
  { slName :: CString
  , slFunction :: CString
  , slFile :: CString
  , slLine :: Word32
  , slColor :: Word32
  }

instance Storable SourceLocationData where
  sizeOf _ = (#size struct ___tracy_source_location_data)
  alignment _ = (#alignment struct ___tracy_source_location_data)
  peek ptr = SourceLocationData
    <$> (#peek struct ___tracy_source_location_data, name) ptr
    <*> (#peek struct ___tracy_source_location_data, function) ptr
    <*> (#peek struct ___tracy_source_location_data, file) ptr
    <*> (#peek struct ___tracy_source_location_data, line) ptr
    <*> (#peek struct ___tracy_source_location_data, color) ptr
  poke ptr (SourceLocationData name function file line color) = do
    (#poke struct ___tracy_source_location_data, name) ptr name
    (#poke struct ___tracy_source_location_data, function) ptr function
    (#poke struct ___tracy_source_location_data, file) ptr file
    (#poke struct ___tracy_source_location_data, line) ptr line
    (#poke struct ___tracy_source_location_data, color) ptr color

#ifdef HS_MANUAL_LIFETIME
foreign import ccall unsafe "tracy_wrapper.c tracy_startup_profiler" startupProfiler :: IO ()
foreign import ccall unsafe "tracy_wrapper.c tracy_shutdown_profiler" shutdownProfiler :: IO ()
#endif

withProfiler :: IO a -> IO a
#ifdef HS_MANUAL_LIFETIME
withProfiler = bracket startupProfiler (const shutdownProfiler) . const
#else
withProfiler = id
#endif

foreign import ccall unsafe "___tracy_alloc_srcloc" c_alloc_srcloc
  :: Word32
  -> Ptr CChar -> CSize
  -> CString -> CSize
  -> Word32
  -> IO Word64

allocSrcLoc
  :: Int
  -> String
  -> String
  -> Word32
  -> IO Word64
allocSrcLoc line source function color =
  withCStringLen source \(sourcePtr, sourceSz) ->
    withCStringLen function \(functionPtr, functionSz) ->
      c_alloc_srcloc
        (fromIntegral line)
        sourcePtr
        (fromIntegral sourceSz)
        functionPtr
        (fromIntegral functionSz)
        color

newtype TracyCZoneCtx = TracyCZoneCtx (Ptr TracyCZoneCtx)
foreign import ccall "tracy_wrapper.c tracy_zone_begin_alloc" c_tracy_zone_begin_alloc
    :: Word64 -> CInt -> IO (Ptr TracyCZoneCtx)
foreign import ccall "tracy_wrapper.c tracy_zone_begin" c_tracy_zone_begin
    :: Ptr SourceLocationData -> CInt -> IO (Ptr TracyCZoneCtx)
foreign import ccall "tracy_wrapper.c tracy_zone_end" c_tracy_zone_end
    :: Ptr TracyCZoneCtx -> IO ()
foreign import ccall "tracy_wrapper.c tracy_emit_message" c_tracy_emit_message
    :: CString -> CSize -> CInt -> IO ()
foreign import ccall "tracy_wrapper.c tracy_emit_messageL" c_tracy_emit_messageL
    :: CString -> CInt -> IO ()
foreign import ccall "tracy_wrapper.c tracy_emit_zone_text" c_tracy_emit_zone_text
    :: Ptr TracyCZoneCtx -> CString -> CSize -> IO ()
foreign import ccall "tracy_wrapper.c tracy_emit_zone_name" c_tracy_emit_zone_name
    :: Ptr TracyCZoneCtx -> CString -> CSize -> IO ()
foreign import ccall "tracy_wrapper.c tracy_connected" c_tracy_connected
    :: IO CInt
foreign import ccall "tracy_wrapper.c tracy_set_thread_name" c_tracy_set_thread_name
    :: CString -> IO ()
foreign import ccall "tracy_wrapper.c tracy_frame_mark" c_frame_mark
    :: CString -> IO ()
foreign import ccall "tracy_wrapper.c tracy_memory_alloc" c_memory_alloc
    :: Ptr () -> CSize -> CInt -> IO ()
foreign import ccall "tracy_wrapper.c tracy_memory_free" c_memory_free
    :: Ptr () -> CInt -> IO ()
foreign import ccall "tracy_wrapper.c tracy_emit_plot" c_emit_plot
    :: CString -> Double -> IO ()

isConnected :: IO Bool
isConnected = (/= 0) <$> c_tracy_connected

zoneName :: TracyCZoneCtx -> String -> IO ()
zoneName (TracyCZoneCtx ctx) s = withCStringLen s \(ptr, len) ->
  c_tracy_emit_zone_name ctx ptr (fromIntegral len)

zoneText :: TracyCZoneCtx -> String -> IO ()
zoneText (TracyCZoneCtx ctx) s = withCStringLen s \(ptr, len) ->
  c_tracy_emit_zone_text ctx ptr (fromIntegral len)

withZone :: Word64 -> Bool -> (TracyCZoneCtx -> IO a) -> IO a
withZone srcloc active = bracket
  (TracyCZoneCtx <$> c_tracy_zone_begin_alloc srcloc (fromBool active))
  (\(TracyCZoneCtx ctx) -> c_tracy_zone_end ctx)

withZoneSRCLOC :: Int -> String -> String -> String -> Word32 -> Bool -> (TracyCZoneCtx -> IO a) -> IO a
withZoneSRCLOC line file function name color active f =
  withCString name \namePtr ->
    withCString function \functionPtr ->
      withCString file \filePtr ->
        let sl = SourceLocationData namePtr functionPtr filePtr (fromIntegral line) color
        in with sl \slPtr ->
            bracket
              (TracyCZoneCtx <$> c_tracy_zone_begin slPtr (fromBool active))
              (\(TracyCZoneCtx ctx) -> c_tracy_zone_end ctx)
              f

message :: String -> Int -> IO ()
message s depth = withCStringLen s \(ptr, len) ->
  c_tracy_emit_message ptr (fromIntegral len) (fromIntegral depth)

messageL :: String -> Int -> IO ()
messageL s depth = withCString s \ptr -> c_tracy_emit_messageL ptr (fromIntegral depth)

setThreadName :: String -> IO ()
setThreadName s = withCString s c_tracy_set_thread_name

frameMark :: Maybe String -> IO ()
frameMark Nothing = c_frame_mark nullPtr
frameMark (Just s) = withCString s c_frame_mark

memoryAlloc :: Ptr a -> Int -> Bool -> IO ()
memoryAlloc ptr size secure = c_memory_alloc (castPtr ptr) (fromIntegral size) (fromBool secure)

memoryFree :: Ptr a -> Bool -> IO ()
memoryFree ptr secure = c_memory_free (castPtr ptr) (fromBool secure)

plotData :: String -> Double -> IO ()
plotData name val = withCString name \namePtr ->
  c_emit_plot namePtr val

waitConnected :: Int -> Maybe Int -> IO Bool
waitConnected interval mtimeout = fromMaybe False <$> case mtimeout of
    Nothing -> fmap Just go
    Just t -> timeout t go
  where
    go = do
      conn <- isConnected
      if conn
        then pure True
        else threadDelay interval >> go
