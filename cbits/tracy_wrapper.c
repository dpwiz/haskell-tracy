#include <tracy/TracyC.h>
#include <stdlib.h>

void tracy_startup_profiler() {
  ___tracy_startup_profiler();
}

void tracy_shutdown_profiler() {
  ___tracy_shutdown_profiler();
}

TracyCZoneCtx* tracy_zone_begin_alloc(uint64_t srcloc) {
  TracyCZoneCtx* ctx = (TracyCZoneCtx*)malloc(sizeof(TracyCZoneCtx));
  *ctx = ___tracy_emit_zone_begin_alloc(srcloc, 1);
  return ctx;
}

void tracy_zone_end(TracyCZoneCtx* ctx) {
  ___tracy_emit_zone_end(*ctx);
  free(ctx);
}

void tracy_emit_message(const char* txt, size_t size) {
  ___tracy_emit_message(txt, size, 0);
}

void tracy_emit_zone_text(TracyCZoneCtx* ctx, const char* txt, size_t size) {
  ___tracy_emit_zone_text(*ctx, txt, size);
}

void tracy_emit_zone_name(TracyCZoneCtx* ctx, const char* txt, size_t size) {
  ___tracy_emit_zone_name(*ctx, txt, size);
}

void tracy_emit_messageL(const char* txt) {
  ___tracy_emit_messageL(txt, 0);
}
