{-# LANGUAGE BlockArguments #-}

module System.Tracy.Bindings where

import Data.Word
import Foreign.C
import Foreign.Ptr

#include <tracy/TracyC.h>

{- XXX: requires passing TRACY_MANUAL_LIFETIME somewhere

foreign import ccall unsafe "___tracy_profiler_started" c_tracy_profiler_started :: IO CInt

profilerStarted :: IO Bool
profilerStarted = (/= 0) <$> c_tracy_profiler_started
-}

-- TRACY_API uint64_t ___tracy_alloc_srcloc( uint32_t line, const char* source, size_t sourceSz, const char* function, size_t functionSz, uint32_t color );
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

-- TODO: TRACY_API TracyCZoneCtx ___tracy_emit_zone_begin_alloc( uint64_t srcloc, int active );
-- TODO: TRACY_API void ___tracy_emit_zone_end( TracyCZoneCtx ctx );
-- TODO: TRACY_API void ___tracy_emit_message( const char* txt, size_t size, int callstack );

-- TODO: TRACY_API void ___tracy_emit_zone_text( TracyCZoneCtx ctx, const char* txt, size_t size );
-- TODO: TRACY_API void ___tracy_emit_zone_name( TracyCZoneCtx ctx, const char* txt, size_t size );
-- TODO: TRACY_API void ___tracy_emit_zone_color( TracyCZoneCtx ctx, uint32_t color );
-- TODO: TRACY_API void ___tracy_emit_zone_value( TracyCZoneCtx ctx, uint64_t value );

-- TODO: the rest of the TRACY_API...
