/* SPDX-License-Identifier: MIT */
#include <sdkconfig.h>

#ifdef CONFIG_AVM_ESP_ADF_AUDIO_ELEMENT_ENABLE

#include <stdlib.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"

#include <esp_attr.h>
#include <esp_log.h>
#include <esp_system.h>

#include <audio_element.h>

#pragma GCC diagnostic pop

#include <context.h>
#include <defaultatoms.h>
#include <erl_nif.h>
#include <erl_nif_priv.h>
#include <esp32_sys.h>
#include <globalcontext.h>
#include <interop.h>
#include <memory.h>
#include <nifs.h>
#include <resources.h>
#include <smp.h>
#include <term.h>
#include <utils.h>

// #define ENABLE_TRACE
#include <trace.h>

#include "atomvm_esp_adf_audio_element.h"

#define MODULE_PREFIX "esp_adf_audio_element:"

//
// Resources
//

static void audio_element_dtor(ErlNifEnv *caller_env, void *obj)
{
    TRACE("%s:%s\n", __FILE__, __func__);
    UNUSED(caller_env);

    struct AudioElementResource *rsrc_obj = (struct AudioElementResource *) obj;
    struct RefcBinary *rsrc_refc = refc_binary_from_data(rsrc_obj);
    sys_unregister_listener(rsrc_refc->resource_type->global, &rsrc_obj->event_listener);
#ifndef AVM_NO_SMP
    smp_mutex_destroy(rsrc_obj->mutex);
#endif
    audio_element_deinit(rsrc_obj->audio_element);
}

static void audio_element_down(ErlNifEnv *caller_env, void *obj, ErlNifPid *pid, ErlNifMonitor *mon)
{
    TRACE("%s:%s\n", __FILE__, __func__);
    UNUSED(caller_env);
    UNUSED(mon);

    struct AudioElementResource *rsrc_obj = (struct AudioElementResource *) obj;
    if (rsrc_obj->owner_process_id == *pid && rsrc_obj->active) {
        rsrc_obj->owner_process_id = INVALID_PROCESS_ID;
        // Decrement reference count as the audio element can now be gc'd
        struct RefcBinary *rsrc_refc = refc_binary_from_data(rsrc_obj);
        GlobalContext *global = rsrc_refc->resource_type->global;
        refc_binary_decrement_refcount(rsrc_refc, global);
    }
}

static const ErlNifResourceTypeInit audio_element_resource_type_init = {
    .members = 3,
    .dtor = audio_element_dtor,
    .stop = NULL,
    .down = audio_element_down
};

ErlNifResourceType *audio_element_resource_type;

struct AudioElementResource *atomvm_esp_adf_audio_element_opaque_to_resource(term opaque, Context *ctx)
{
    TRACE("%s:%s\n", __FILE__, __func__);
    if (UNLIKELY(!term_is_tuple(opaque) || term_get_tuple_arity(opaque) != 2 || !term_is_reference(term_get_tuple_element(opaque, 0)))) {
        return NULL;
    }
    term resource_term = term_get_tuple_element(opaque, 1);
    void *rsrc_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), resource_term, audio_element_resource_type, &rsrc_obj_ptr))) {
        return NULL;
    }
    return (struct AudioElementResource *) rsrc_obj_ptr;
}

static term atomvm_esp_adf_audio_element_resource_to_opaque_opt(struct AudioElementResource *resource, Heap *heap, bool incr_refc)
{
    TRACE("%s:%s\n", __FILE__, __func__);
    term obj = term_from_resource(resource, heap);
    if (incr_refc) {
        struct RefcBinary *rsrc_refc = refc_binary_from_data(resource);
        refc_binary_increment_refcount(rsrc_refc);
    }

    term opaque_term = term_alloc_tuple(2, heap);
    term ref = term_from_ref_ticks(resource->ref_ticks, heap);
    term_put_tuple_element(opaque_term, 0, ref);
    term_put_tuple_element(opaque_term, 1, obj);

    return opaque_term;
}

term atomvm_esp_adf_audio_element_resource_to_opaque(struct AudioElementResource *resource, Context *ctx)
{
    TRACE("%s:%s\n", __FILE__, __func__);
    // For a nif result, we don't need to increment the ref count
    return atomvm_esp_adf_audio_element_resource_to_opaque_opt(resource, &ctx->heap, false);
}

//
// Events
//

static term esp_codec_to_term(esp_codec_type_t esp_codec, GlobalContext *glb)
{
    static const AtomStringIntPair codec_atoms[] = {
        { ATOM_STR("\x7", "unknown"), ESP_CODEC_TYPE_UNKNOW },
        { ATOM_STR("\x3", "raw"), ESP_CODEC_TYPE_RAW },
        { ATOM_STR("\x3", "wav"), ESP_CODEC_TYPE_WAV },
        { ATOM_STR("\x3", "mp3"), ESP_CODEC_TYPE_MP3 },
        { ATOM_STR("\x3", "aac"), ESP_CODEC_TYPE_AAC },
        { ATOM_STR("\x4", "opus"), ESP_CODEC_TYPE_OPUS },
        { ATOM_STR("\x3", "m4a"), ESP_CODEC_TYPE_M4A },
        { ATOM_STR("\x3", "mp4"), ESP_CODEC_TYPE_MP4 },
        { ATOM_STR("\x4", "flac"), ESP_CODEC_TYPE_FLAC },
        { ATOM_STR("\x3", "ogg"), ESP_CODEC_TYPE_OGG },
        { ATOM_STR("\x5", "tsaac"), ESP_CODEC_TYPE_TSAAC },
        { ATOM_STR("\x5", "amrnb"), ESP_CODEC_TYPE_AMRNB },
        { ATOM_STR("\x5", "amrwb"), ESP_CODEC_TYPE_AMRWB },
        { ATOM_STR("\x3", "pcm"), ESP_CODEC_TYPE_PCM },
        { ATOM_STR("\x4", "m3u8"), ESP_AUDIO_TYPE_M3U8 },
        { ATOM_STR("\x3", "pls"), ESP_AUDIO_TYPE_PLS },
        { ATOM_STR("\xB", "unsupported"), ESP_CODEC_TYPE_UNSUPPORT },
        SELECT_INT_DEFAULT(-1)
    };

    term result = interop_atom_term_select_atom(codec_atoms, esp_codec, glb);
    if (term_is_invalid_term(result)) {
        result = term_from_int(esp_codec);
    }
    return result;
}

static term info_to_term(audio_element_info_t *info, Heap *heap, GlobalContext *glb)
{
    term info_list = term_nil();

    term tuple = term_alloc_tuple(2, heap);
    term_put_tuple_element(tuple, 0, globalcontext_make_atom(glb, ATOM_STR("\xC", "sample_rates")));
    term_put_tuple_element(tuple, 1, term_from_int(info->sample_rates));
    info_list = term_list_prepend(tuple, info_list, heap);

    tuple = term_alloc_tuple(2, heap);
    term_put_tuple_element(tuple, 0, globalcontext_make_atom(glb, ATOM_STR("\x8", "channels")));
    term_put_tuple_element(tuple, 1, term_from_int(info->channels));
    info_list = term_list_prepend(tuple, info_list, heap);

    tuple = term_alloc_tuple(2, heap);
    term_put_tuple_element(tuple, 0, globalcontext_make_atom(glb, ATOM_STR("\x4", "bits")));
    term_put_tuple_element(tuple, 1, term_from_int(info->bits));
    info_list = term_list_prepend(tuple, info_list, heap);

    tuple = term_alloc_tuple(2, heap);
    term_put_tuple_element(tuple, 0, globalcontext_make_atom(glb, ATOM_STR("\x3", "bps")));
    term_put_tuple_element(tuple, 1, term_from_int(info->bps));
    info_list = term_list_prepend(tuple, info_list, heap);

    tuple = term_alloc_tuple(2, heap);
    term_put_tuple_element(tuple, 0, globalcontext_make_atom(glb, ATOM_STR("\x8", "byte_pos")));
    term_put_tuple_element(tuple, 1, term_from_int(info->byte_pos));
    info_list = term_list_prepend(tuple, info_list, heap);

    tuple = term_alloc_tuple(2, heap);
    term_put_tuple_element(tuple, 0, globalcontext_make_atom(glb, ATOM_STR("\xB", "total_bytes")));
    term_put_tuple_element(tuple, 1, term_from_int(info->total_bytes));
    info_list = term_list_prepend(tuple, info_list, heap);

    tuple = term_alloc_tuple(2, heap);
    term_put_tuple_element(tuple, 0, globalcontext_make_atom(glb, ATOM_STR("\x8", "duration")));
    term_put_tuple_element(tuple, 1, term_from_int(info->duration));
    info_list = term_list_prepend(tuple, info_list, heap);

    tuple = term_alloc_tuple(2, heap);
    term_put_tuple_element(tuple, 0, globalcontext_make_atom(glb, ATOM_STR("\x9", "codec_fmt")));
    term_put_tuple_element(tuple, 1, esp_codec_to_term(info->codec_fmt, glb));
    info_list = term_list_prepend(tuple, info_list, heap);

    return info_list;
}

static term status_to_term(audio_element_status_t status, GlobalContext *glb)
{
    static const AtomStringIntPair status_atoms[] = {
        { ATOM_STR("\x4", "none"), AEL_STATUS_NONE },
        { ATOM_STR("\xA", "error_open"), AEL_STATUS_ERROR_OPEN },
        { ATOM_STR("\xB", "error_input"), AEL_STATUS_ERROR_INPUT },
        { ATOM_STR("\xD", "error_process"), AEL_STATUS_ERROR_PROCESS },
        { ATOM_STR("\xC", "error_output"), AEL_STATUS_ERROR_OUTPUT },
        { ATOM_STR("\xB", "error_close"), AEL_STATUS_ERROR_CLOSE },
        { ATOM_STR("\xD", "error_timeout"), AEL_STATUS_ERROR_TIMEOUT },
        { ATOM_STR("\xD", "error_unknown"), AEL_STATUS_ERROR_UNKNOWN },
        { ATOM_STR("\xA", "input_done"), AEL_STATUS_INPUT_DONE },
        { ATOM_STR("\xF", "input_buffering"), AEL_STATUS_INPUT_BUFFERING },
        { ATOM_STR("\xB", "output_done"), AEL_STATUS_OUTPUT_DONE },
        { ATOM_STR("\x10", "output_buffering"), AEL_STATUS_OUTPUT_BUFFERING },
        { ATOM_STR("\xD", "state_running"), AEL_STATUS_STATE_RUNNING },
        { ATOM_STR("\xC", "state_paused"), AEL_STATUS_STATE_PAUSED },
        { ATOM_STR("\xD", "state_stopped"), AEL_STATUS_STATE_STOPPED },
        { ATOM_STR("\xE", "state_finished"), AEL_STATUS_STATE_FINISHED },
        { ATOM_STR("\x7", "mounted"), AEL_STATUS_MOUNTED },
        { ATOM_STR("\x9", "unmounted"), AEL_STATUS_UNMOUNTED },
        SELECT_INT_DEFAULT(-1)
    };

    term result = interop_atom_term_select_atom(status_atoms, status, glb);
    if (term_is_invalid_term(result)) {
        result = term_from_int(status);
    }
    return result;
}

static term event_cmd_to_term(int event_cmd, GlobalContext *glb)
{
    static const AtomStringIntPair event_cmd_atoms[] = {
        { ATOM_STR("\xA", "music_info"), AEL_MSG_CMD_REPORT_MUSIC_INFO },
        { ATOM_STR("\x9", "codec_fmt"), AEL_MSG_CMD_REPORT_CODEC_FMT },
        { ATOM_STR("\x6", "status"), AEL_MSG_CMD_REPORT_STATUS },
        { ATOM_STR("\x8", "position"), AEL_MSG_CMD_REPORT_POSITION },
        SELECT_INT_DEFAULT(-1)
    };

    term result = interop_atom_term_select_atom(event_cmd_atoms, event_cmd, glb);
    if (term_is_invalid_term(result)) {
        result = term_from_int(event_cmd);
    }
    return result;
}

static size_t event_heap_size_in_terms(audio_event_iface_msg_t *msg)
{
    switch (msg->cmd) {
        case AEL_MSG_CMD_REPORT_STATUS:
            return TUPLE_SIZE(2);
            break;

        case AEL_MSG_CMD_REPORT_POSITION:
            return TUPLE_SIZE(2) + LIST_SIZE(8, TUPLE_SIZE(2));
            break;

        default:
            break;
    }
    return 0;
}

static term create_event_term(audio_event_iface_msg_t *msg, Heap *heap, GlobalContext *glb)
{
    term event_term;
    switch (msg->cmd) {
        case AEL_MSG_CMD_REPORT_STATUS:
            event_term = term_alloc_tuple(2, heap);
            term_put_tuple_element(event_term, 0, event_cmd_to_term(msg->cmd, glb));
            term_put_tuple_element(event_term, 1, status_to_term((audio_element_status_t) msg->data, glb));
            break;

        case AEL_MSG_CMD_REPORT_POSITION:
            event_term = term_alloc_tuple(2, heap);
            term_put_tuple_element(event_term, 0, event_cmd_to_term(msg->cmd, glb));
            term_put_tuple_element(event_term, 1, info_to_term((audio_element_info_t *) msg->data, heap, glb));
            break;

        default:
            event_term = event_cmd_to_term(msg->cmd, glb);
    }
    return event_term;
}

static void send_active_event(struct AudioElementResource *resource, audio_event_iface_msg_t *msg, GlobalContext *glb)
{
    TRACE("%s:%d\n", __FILE__, __LINE__);

    Context *ctx = globalcontext_get_process_lock(glb, resource->owner_process_id);
    if (IS_NULL_PTR(ctx)) {
        // We have a monitor for owner, so this is unlikely
        if (msg->need_free_data) {
            free(msg->data);
        }
        return;
    }

    size_t message_size = TUPLE_SIZE(3) + AUDIO_ELEMENT_OPAQUE_TERM_SIZE;
    message_size += event_heap_size_in_terms(msg);

    Heap heap;
    if (UNLIKELY(memory_init_heap(&heap, message_size) != MEMORY_GC_OK)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        AVM_ABORT();
    }

    term event_term = create_event_term(msg, &heap, glb);
    term opaque_ref = atomvm_esp_adf_audio_element_resource_to_opaque_opt(resource, &heap, true);
    term audio_element_atom = globalcontext_make_atom(glb, ATOM_STR("\xD", "audio_element"));
    term msg_term = term_alloc_tuple(3, &heap);
    term_put_tuple_element(msg_term, 0, audio_element_atom);
    term_put_tuple_element(msg_term, 1, opaque_ref);
    term_put_tuple_element(msg_term, 2, event_term);
    globalcontext_send_message_nolock(glb, resource->owner_process_id, msg_term);
    memory_destroy_heap(&heap, glb);
    globalcontext_get_process_unlock(glb, ctx);

    if (msg->need_free_data) {
        free(msg->data);
    }
}

static EventListener *atomvm_esp_adf_audio_element_event_handler(GlobalContext *glb, EventListener *listener)
{
    TRACE("%s:%s\n", __FILE__, __func__);
    struct AudioElementResource *rsrc_obj = CONTAINER_OF(listener, struct AudioElementResource, event_listener);

    // On SMP builds, this handler can be called from another scheduler.
    SMP_MUTEX_LOCK(rsrc_obj->mutex);
    if (rsrc_obj->active) {
        // Send event to owner process
        audio_event_iface_msg_t msg;
        if (xQueueReceive(listener->sender, &msg, 0)) {
            send_active_event(rsrc_obj, &msg, glb);
        }
    } else if (rsrc_obj->trapped_process_id != INVALID_PROCESS_ID) {
        // Send event to blocked reader
        audio_event_iface_msg_t msg;
        if (xQueueReceive(listener->sender, &msg, 0)) {
            size_t requested_size = TUPLE_SIZE(2) + event_heap_size_in_terms(&msg);

            Heap heap;
            if (UNLIKELY(memory_init_heap(&heap, requested_size) != MEMORY_GC_OK)) {
                fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
                AVM_ABORT();
            }

            term event_term = create_event_term(&msg, &heap, glb);
            term result = term_alloc_tuple(2, &heap);
            term_put_tuple_element(result, 0, OK_ATOM);
            term_put_tuple_element(result, 1, event_term);

            Context *ctx = globalcontext_get_process_lock(glb, rsrc_obj->trapped_process_id);
            mailbox_send_term_signal(ctx, TrapAnswerSignal, result);
            globalcontext_get_process_unlock(glb, ctx);

            if (msg.need_free_data) {
                free(msg.data);
            }
        }
        rsrc_obj->trapped_process_id = INVALID_PROCESS_ID;

        // Decrement ref counter as process is no longer trapped
        struct RefcBinary *rsrc_refc = refc_binary_from_data(rsrc_obj);
        refc_binary_decrement_refcount(rsrc_refc, glb);
    }
    SMP_MUTEX_UNLOCK(rsrc_obj->mutex);
    return listener;
}

static void atomvm_esp_adf_audio_element_set_active(struct AudioElementResource *resource, bool active, GlobalContext *global)
{
    TRACE("%s:%s\n", __FILE__, __func__);
    if (resource->active != active) {
        resource->active = active;
        if (active) {
            // Increment ref count
            struct RefcBinary *rsrc_refc = refc_binary_from_data(resource);
            refc_binary_increment_refcount(rsrc_refc);
            // Register for events
            xQueueAddToSet(resource->event_listener.sender, event_set);
        } else {
            // Decrement ref count
            struct RefcBinary *rsrc_refc = refc_binary_from_data(resource);
            refc_binary_decrement_refcount(rsrc_refc, global);
            // Unregister for events
            xQueueRemoveFromSet(resource->event_listener.sender, event_set);
        }
    }
}

void atomvm_esp_adf_audio_element_init_resource(struct AudioElementResource *resource, audio_element_handle_t audio_element, bool active, Context *ctx)
{
    TRACE("%s:%s\n", __FILE__, __func__);
    resource->audio_element = audio_element;
    resource->ref_ticks = globalcontext_get_ref_ticks(ctx->global);
    resource->event_listener.handler = atomvm_esp_adf_audio_element_event_handler;
    resource->event_listener.sender = audio_element_get_event_queue(audio_element);
    sys_register_listener(ctx->global, &resource->event_listener);
    resource->owner_process_id = ctx->process_id;
    enif_monitor_process(erl_nif_env_from_context(ctx), resource, &ctx->process_id, &resource->owner_process_monitor);
    resource->active = false;
    resource->trapped_process_id = INVALID_PROCESS_ID;
#ifndef AVM_NO_SMP
    resource->mutex = smp_mutex_create();
#endif
    atomvm_esp_adf_audio_element_set_active(resource, active, ctx->global);
}

//
// Nifs
//

struct BinaryCursor
{
    GlobalContext *global;
    struct RefcBinary *refc;
    const char *ptr;
    const char *end;
};

static int read_binary_cb(audio_element_handle_t el, char *buf, int len, TickType_t wait_time, void *ctx)
{
    TRACE("%s:%s\n", __FILE__, __func__);
    UNUSED(el);
    UNUSED(wait_time);

    struct BinaryCursor *cursor = (struct BinaryCursor *) ctx;
    int read_size = cursor->end - cursor->ptr;
    if (read_size == 0) {
        GlobalContext *global = cursor->global;
        if (cursor->refc) {
            globalcontext_refc_decrement_refcount_from_task(global, cursor->refc);
        }
        struct RefcBinary *cursor_refc = refc_binary_from_data(cursor);
        globalcontext_refc_decrement_refcount_from_task(global, cursor_refc);
        return AEL_IO_DONE;
    } else if (len < read_size) {
        read_size = len;
    }
    memcpy(buf, cursor->ptr, read_size);
    cursor->ptr += read_size;
    return read_size;
}

static term nif_set_read_binary(Context *ctx, int argc, term argv[])
{
    TRACE("%s:%s\n", __FILE__, __func__);
    UNUSED(argc);

    VALIDATE_VALUE(argv[1], term_is_binary);

    struct AudioElementResource *rsrc_obj = atomvm_esp_adf_audio_element_opaque_to_resource(argv[0], ctx);
    if (IS_NULL_PTR(rsrc_obj)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    // Ensure argv[1] is a refc binary that we can lock.
    size_t binary_size = term_binary_size(argv[1]);
    term data_bin;
    if (term_is_refc_binary(argv[1])) {
        data_bin = argv[1];
    } else {
        data_bin = term_alloc_refc_binary(binary_size, false, &ctx->heap, ctx->global);
        memcpy((void *) term_binary_data(data_bin), (const void *) term_binary_data(argv[1]), binary_size);
    }
    // Create a refc binary for the cursor so memory usage is reported.
    struct RefcBinary *cursor_refc = refc_binary_create_refc(sizeof(struct BinaryCursor));
    if (IS_NULL_PTR(cursor_refc)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    struct BinaryCursor *cursor = (struct BinaryCursor *) refc_binary_get_data(cursor_refc);
    refc_binary_increment_refcount(cursor_refc);
    cursor->global = ctx->global;
    if (!term_refc_binary_is_const(data_bin)) {
        struct RefcBinary *refc = term_refc_binary_ptr(data_bin);
        refc_binary_increment_refcount(refc);
        cursor->refc = refc;
    } else {
        cursor->refc = NULL;
    }
    cursor->ptr = term_binary_data(data_bin);
    cursor->end = cursor->ptr + binary_size;

    audio_element_set_read_cb(rsrc_obj->audio_element, read_binary_cb, cursor);

    return OK_ATOM;
}

static term nif_setopts(Context *ctx, int argc, term argv[])
{
    TRACE("%s:%s\n", __FILE__, __func__);
    UNUSED(argc);

    struct AudioElementResource *rsrc_obj = atomvm_esp_adf_audio_element_opaque_to_resource(argv[0], ctx);
    if (IS_NULL_PTR(rsrc_obj)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    term active_term = interop_kv_get_value(argv[1], ATOM_STR("\x6", "active"), ctx->global);

    if (!term_is_invalid_term(active_term)) {
        if (UNLIKELY(active_term != TRUE_ATOM && active_term != FALSE_ATOM)) {
            RAISE_ERROR(BADARG_ATOM);
        }
        SMP_MUTEX_LOCK(rsrc_obj->mutex);
        atomvm_esp_adf_audio_element_set_active(rsrc_obj, active_term == TRUE_ATOM, ctx->global);
        SMP_MUTEX_UNLOCK(rsrc_obj->mutex);
    }

    return OK_ATOM;
}

static term nif_controlling_process(Context *ctx, int argc, term argv[])
{
    TRACE("%s:%s\n", __FILE__, __func__);
    UNUSED(argc);

    struct AudioElementResource *rsrc_obj = atomvm_esp_adf_audio_element_opaque_to_resource(argv[0], ctx);
    if (IS_NULL_PTR(rsrc_obj)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    VALIDATE_VALUE(argv[1], term_is_pid);

    int32_t new_owner_pid = term_to_local_process_id(argv[1]);

    if (ctx->process_id != rsrc_obj->owner_process_id) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        term result = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(result, 0, ERROR_ATOM);
        term_put_tuple_element(result, 1, globalcontext_make_atom(ctx->global, ATOM_STR("\x9", "not_owner")));
        return result;
    }
    if (new_owner_pid != rsrc_obj->owner_process_id) {
        ErlNifEnv *env = erl_nif_env_from_context(ctx);
        enif_demonitor_process(env, rsrc_obj, &rsrc_obj->owner_process_monitor);
        if (UNLIKELY(enif_monitor_process(env, rsrc_obj, &new_owner_pid, &rsrc_obj->owner_process_monitor) != 0)) {
            enif_monitor_process(env, rsrc_obj, &ctx->process_id, &rsrc_obj->owner_process_monitor);
            term result = term_alloc_tuple(2, &ctx->heap);
            term_put_tuple_element(result, 0, ERROR_ATOM);
            term_put_tuple_element(result, 1, BADARG_ATOM);
            return result;
        }
        rsrc_obj->owner_process_id = new_owner_pid;
    }

    return OK_ATOM;
}

static term nif_get_event(Context *ctx, int argc, term argv[])
{
    TRACE("%s:%s\n", __FILE__, __func__);

    struct AudioElementResource *rsrc_obj = atomvm_esp_adf_audio_element_opaque_to_resource(argv[0], ctx);
    if (IS_NULL_PTR(rsrc_obj)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    term timeout = INFINITY_ATOM;
    if (argc > 1 && argv[1] != INFINITY_ATOM) {
        VALIDATE_VALUE(argv[1], term_is_integer);

        timeout = argv[1];
    }

    // For now, we only support 0 or infinity.
    if (timeout != INFINITY_ATOM && timeout != term_from_int(0)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    SMP_MUTEX_LOCK(rsrc_obj->mutex);
    if (UNLIKELY(rsrc_obj->active || rsrc_obj->trapped_process_id != INVALID_PROCESS_ID)) {
        SMP_MUTEX_UNLOCK(rsrc_obj->mutex);

        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        term result = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(result, 0, ERROR_ATOM);
        term_put_tuple_element(result, 1, globalcontext_make_atom(ctx->global, ATOM_STR("\x6", "einval")));
        return result;
    }

    audio_event_iface_msg_t msg;
    if (xQueueReceive(rsrc_obj->event_listener.sender, &msg, 0)) {
        SMP_MUTEX_UNLOCK(rsrc_obj->mutex);
        size_t requested_size = TUPLE_SIZE(2) + event_heap_size_in_terms(&msg);

        if (UNLIKELY(memory_ensure_free(ctx, requested_size) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }

        term event_term = create_event_term(&msg, &ctx->heap, ctx->global);
        term result = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(result, 0, OK_ATOM);
        term_put_tuple_element(result, 1, event_term);

        if (msg.need_free_data) {
            free(msg.data);
        }

        return result;
    }

    if (timeout == term_from_int(0)) {
        SMP_MUTEX_UNLOCK(rsrc_obj->mutex);

        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }

        term result = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(result, 0, ERROR_ATOM);
        term_put_tuple_element(result, 1, globalcontext_make_atom(ctx->global, ATOM_STR("\x7", "timeout")));

        return result;
    }

    // Increment ref counter to ensure resource doesn't go away
    struct RefcBinary *rsrc_refc = refc_binary_from_data(rsrc_obj);
    refc_binary_increment_refcount(rsrc_refc);

    rsrc_obj->trapped_process_id = ctx->process_id;
    context_update_flags(ctx, ~NoFlags, Trap);

    xQueueAddToSet(rsrc_obj->event_listener.sender, event_set);

    SMP_MUTEX_UNLOCK(rsrc_obj->mutex);

    return term_invalid_term();
}

static const struct Nif set_read_binary_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_set_read_binary
};
static const struct Nif setopts_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_setopts
};
static const struct Nif controlling_process_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_controlling_process
};
static const struct Nif get_event_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_get_event
};

//
// Component Nif Entrypoints
//

static const struct Nif *get_nif(const char *nifname)
{
    if (memcmp(nifname, MODULE_PREFIX, strlen(MODULE_PREFIX))) {
        return NULL;
    }
    if (strcmp(nifname + strlen(MODULE_PREFIX), "set_read_binary/2") == 0) {
        return &set_read_binary_nif;
    }
    if (strcmp(nifname + strlen(MODULE_PREFIX), "setopts/2") == 0) {
        return &setopts_nif;
    }
    if (strcmp(nifname + strlen(MODULE_PREFIX), "controlling_process/2") == 0) {
        return &controlling_process_nif;
    }
    if (strcmp(nifname + strlen(MODULE_PREFIX), "get_event/1") == 0) {
        return &get_event_nif;
    }
    if (strcmp(nifname + strlen(MODULE_PREFIX), "get_event/2") == 0) {
        return &get_event_nif;
    }
    return NULL;
}

static void esp_adf_audio_element_init(GlobalContext *global)
{
    TRACE("%s:%s\n", __FILE__, __func__);

    ErlNifEnv env;
    erl_nif_env_partial_init_from_globalcontext(&env, global);
    audio_element_resource_type = enif_init_resource_type(&env, "audio_element", &audio_element_resource_type_init, ERL_NIF_RT_CREATE, NULL);
}

REGISTER_NIF_COLLECTION(esp_adf_audio_element, esp_adf_audio_element_init, NULL, get_nif)

#endif
