# tracy-profiler

## Building

Before building the project, you need to build the `TracyClient` library.
This can be done by running the following commands:

```sh
cmake -B upstream/tracy/build -S upstream/tracy -DTRACY_ENABLE=ON -DTRACY_MANUAL_LIFETIME=ON -DTRACY_DELAYED_INIT=ON -DTRACY_ONLY_LOCALHOST=ON -DCMAKE_POSITION_INDEPENDENT_CODE=ON
cmake --build upstream/tracy/build --config Release
```

If you are using a system-installed version of Tracy, you can disable the `manual_lifetime` flag in `package.yaml` and run the following command instead:

```sh
cmake -B upstream/tracy/build -S upstream/tracy -DTRACY_ENABLE=ON -DTRACY_ONLY_LOCALHOST=ON -DCMAKE_POSITION_INDEPENDENT_CODE=ON
cmake --build upstream/tracy/build --config Release
```

## API Coverage

- [ ] `___tracy_set_thread_name`
- [x] `___tracy_startup_profiler`
- [x] `___tracy_shutdown_profiler`
- [ ] `___tracy_profiler_started`
- [x] `___tracy_alloc_srcloc`
- [ ] `___tracy_alloc_srcloc_name`
- [ ] `___tracy_emit_zone_begin`
- [ ] `___tracy_emit_zone_begin_callstack`
- [x] `___tracy_emit_zone_begin_alloc`
- [ ] `___tracy_emit_zone_begin_alloc_callstack`
- [x] `___tracy_emit_zone_end`
- [x] `___tracy_emit_zone_text`
- [x] `___tracy_emit_zone_name`
- [ ] `___tracy_emit_zone_color`
- [ ] `___tracy_emit_zone_value`
- [ ] `___tracy_emit_gpu_zone_begin`
- [ ] `___tracy_emit_gpu_zone_begin_callstack`
- [ ] `___tracy_emit_gpu_zone_begin_alloc`
- [ ] `___tracy_emit_gpu_zone_begin_alloc_callstack`
- [ ] `___tracy_emit_gpu_zone_end`
- [ ] `___tracy_emit_gpu_time`
- [ ] `___tracy_emit_gpu_new_context`
- [ ] `___tracy_emit_gpu_context_name`
- [ ] `___tracy_emit_gpu_calibration`
- [ ] `___tracy_emit_gpu_time_sync`
- [ ] `___tracy_emit_gpu_zone_begin_serial`
- [ ] `___tracy_emit_gpu_zone_begin_callstack_serial`
- [ ] `___tracy_emit_gpu_zone_begin_alloc_serial`
- [ ] `___tracy_emit_gpu_zone_begin_alloc_callstack_serial`
- [ ] `___tracy_emit_gpu_zone_end_serial`
- [ ] `___tracy_emit_gpu_time_serial`
- [ ] `___tracy_emit_gpu_new_context_serial`
- [ ] `___tracy_emit_gpu_context_name_serial`
- [ ] `___tracy_emit_gpu_calibration_serial`
- [ ] `___tracy_emit_gpu_time_sync_serial`
- [x] `___tracy_connected`
- [ ] `___tracy_emit_memory_alloc`
- [ ] `___tracy_emit_memory_alloc_callstack`
- [ ] `___tracy_emit_memory_free`
- [ ] `___tracy_emit_memory_free_callstack`
- [ ] `___tracy_emit_memory_alloc_named`
- [ ] `___tracy_emit_memory_alloc_callstack_named`
- [ ] `___tracy_emit_memory_free_named`
- [ ] `___tracy_emit_memory_free_callstack_named`
- [x] `___tracy_emit_message`
- [x] `___tracy_emit_messageL`
- [ ] `___tracy_emit_messageC`
- [ ] `___tracy_emit_messageLC`
- [ ] `___tracy_emit_frame_mark`
- [ ] `___tracy_emit_frame_mark_start`
- [ ] `___tracy_emit_frame_mark_end`
- [ ] `___tracy_emit_frame_image`
- [ ] `___tracy_emit_plot`
- [ ] `___tracy_emit_plot_float`
- [ ] `___tracy_emit_plot_int`
- [ ] `___tracy_emit_plot_config`
- [ ] `___tracy_emit_message_appinfo`
- [ ] `___tracy_announce_lockable_ctx`
- [ ] `___tracy_terminate_lockable_ctx`
- [ ] `___tracy_before_lock_lockable_ctx`
- [ ] `___tracy_after_lock_lockable_ctx`
- [ ] `___tracy_after_unlock_lockable_ctx`
- [ ] `___tracy_after_try_lock_lockable_ctx`
- [ ] `___tracy_mark_lockable_ctx`
- [ ] `___tracy_custom_name_lockable_ctx`
- [ ] `___tracy_fiber_enter`
- [ ] `___tracy_fiber_leave`
