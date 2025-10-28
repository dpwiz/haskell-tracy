#include <tracy/TracyC.h>
#include <stdlib.h>

#ifdef TRACY_MANUAL_LIFETIME
void tracy_startup_profiler() {
  ___tracy_startup_profiler();
}

void tracy_shutdown_profiler() {
  ___tracy_shutdown_profiler();
}
#endif

TracyCZoneCtx* tracy_zone_begin_alloc(uint64_t srcloc) {
  TracyCZoneCtx* ctx = (TracyCZoneCtx*)malloc(sizeof(TracyCZoneCtx));
  *ctx = ___tracy_emit_zone_begin_alloc(srcloc, 1);
  return ctx;
}

TracyCZoneCtx* tracy_zone_begin(const struct ___tracy_source_location_data* srcloc) {
    TracyCZoneCtx* ctx = (TracyCZoneCtx*)malloc(sizeof(TracyCZoneCtx));
    *ctx = ___tracy_emit_zone_begin(srcloc, 1);
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

int tracy_connected() {
  return ___tracy_connected();
}

void tracy_set_thread_name(const char* name) {
  ___tracy_set_thread_name(name);
}

void tracy_frame_mark() {
  ___tracy_emit_frame_mark(NULL);
}

void tracy_memory_alloc(const void* ptr, size_t size) {
  ___tracy_emit_memory_alloc(ptr, size, 0);
}

void tracy_memory_free(const void* ptr) {
  ___tracy_emit_memory_free(ptr, 0);
}

void tracy_emit_plot(const char* name, double val) {
  ___tracy_emit_plot(name, val);
}
