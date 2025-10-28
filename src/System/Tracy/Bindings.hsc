{-# LANGUAGE BlockArguments #-}

module System.Tracy.Bindings (
  startupProfiler,
  shutdownProfiler,
  allocSrcLoc,
  withZone,
  message,
  messageL,
  zoneText,
  zoneName,
  TracyCZoneCtx(..),
) where

import Control.Exception (bracket)
import Data.Word
import Foreign.C
import Foreign.Ptr

#define TRACY_ENABLE
#include <tracy/TracyC.h>

foreign import ccall unsafe "tracy_wrapper.c tracy_startup_profiler" startupProfiler :: IO ()
foreign import ccall unsafe "tracy_wrapper.c tracy_shutdown_profiler" shutdownProfiler :: IO ()

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
    :: Word64 -> IO (Ptr TracyCZoneCtx)
foreign import ccall "tracy_wrapper.c tracy_zone_end" c_tracy_zone_end
    :: Ptr TracyCZoneCtx -> IO ()
foreign import ccall "tracy_wrapper.c tracy_emit_message" c_tracy_emit_message
    :: CString -> CSize -> IO ()
foreign import ccall "tracy_wrapper.c tracy_emit_messageL" c_tracy_emit_messageL
    :: CString -> IO ()
foreign import ccall "tracy_wrapper.c tracy_emit_zone_text" c_tracy_emit_zone_text
    :: Ptr TracyCZoneCtx -> CString -> CSize -> IO ()
foreign import ccall "tracy_wrapper.c tracy_emit_zone_name" c_tracy_emit_zone_name
    :: Ptr TracyCZoneCtx -> CString -> CSize -> IO ()

zoneName :: TracyCZoneCtx -> String -> IO ()
zoneName (TracyCZoneCtx ctx) s = withCStringLen s \(ptr, len) ->
  c_tracy_emit_zone_name ctx ptr (fromIntegral len)

zoneText :: TracyCZoneCtx -> String -> IO ()
zoneText (TracyCZoneCtx ctx) s = withCStringLen s \(ptr, len) ->
  c_tracy_emit_zone_text ctx ptr (fromIntegral len)

withZone :: Word64 -> (TracyCZoneCtx -> IO a) -> IO a
withZone srcloc = bracket
  (TracyCZoneCtx <$> c_tracy_zone_begin_alloc srcloc)
  (\(TracyCZoneCtx ctx) -> c_tracy_zone_end ctx)

message :: String -> IO ()
message s = withCStringLen s \(ptr, len) ->
  c_tracy_emit_message ptr (fromIntegral len)

messageL :: String -> IO ()
messageL s = withCString s c_tracy_emit_messageL
