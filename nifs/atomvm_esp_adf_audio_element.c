/* SPDX-License-Identifier: MIT */
#include <sdkconfig.h>

#ifdef CONFIG_AVM_ESP_ADF_AUDIO_ELEMENT_ENABLE

#include <stdlib.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"

#include <esp_attr.h>
#include <esp_log.h>
#include <esp_system.h>

#pragma GCC diagnostic pop

#include <context.h>
#include <defaultatoms.h>
#include <erl_nif.h>
#include <erl_nif_priv.h>
#include <esp32_sys.h>
#include <interop.h>
#include <memory.h>
#include <nifs.h>
#include <resources.h>
#include <term.h>

// #define ENABLE_TRACE
#include <trace.h>

#include "atomvm_esp_adf_audio_element.h"

#define MODULE_PREFIX "esp_adf_audio_element:"

//
// Resources
//

static void audio_element_dtor(ErlNifEnv *caller_env, void *obj)
{
    UNUSED(caller_env);

    struct AudioElementResource *rsrc_obj = (struct AudioElementResource *) obj;
    audio_element_deinit(rsrc_obj->audio_element);
}

static const ErlNifResourceTypeInit AudioElementContextResourceTypeInit = {
    .members = 1,
    .dtor = audio_element_dtor,
};

ErlNifResourceType *audio_element_resource_type;

struct BinaryCursor
{
    GlobalContext *global;
    struct RefcBinary *refc;
    const char *ptr;
    const char *end;
};

static int read_binary_cb(audio_element_handle_t el, char *buf, int len, TickType_t wait_time, void *ctx)
{
    UNUSED(el);
    UNUSED(wait_time);

    struct BinaryCursor *cursor = (struct BinaryCursor *) ctx;
    int read_size = cursor->end - cursor->ptr;
    if (read_size == 0) {
        GlobalContext *global = cursor->global;
        if (cursor->refc) {
            refc_binary_decrement_refcount(cursor->refc, global);
        }
        struct RefcBinary *cursor_refc = refc_binary_from_data(cursor);
        refc_binary_decrement_refcount(cursor_refc, global);
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
    TRACE("%s\n", __func__);
    UNUSED(argc);

    VALIDATE_VALUE(argv[1], term_is_binary);

    void *rsrc_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], audio_element_resource_type, &rsrc_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct AudioElementResource *rsrc_obj = (struct AudioElementResource *) rsrc_obj_ptr;

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

static const struct Nif set_read_binary_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_set_read_binary
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
    return NULL;
}

static void esp_adf_audio_element_init(GlobalContext *global)
{
    TRACE("%s\n", __func__);

    ErlNifEnv env;
    erl_nif_env_partial_init_from_globalcontext(&env, global);
    audio_element_resource_type = enif_init_resource_type(&env, "audio_element", &AudioElementContextResourceTypeInit, ERL_NIF_RT_CREATE, NULL);
}

REGISTER_NIF_COLLECTION(esp_adf_audio_element, esp_adf_audio_element_init, NULL, get_nif)

#endif
