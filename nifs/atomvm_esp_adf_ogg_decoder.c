/* SPDX-License-Identifier: MIT */
#include <sdkconfig.h>

#ifdef CONFIG_AVM_ESP_ADF_OGG_DECODER_ENABLE

#include <stdlib.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"

#include <esp_attr.h>
#include <esp_log.h>
#include <esp_system.h>

#include <ogg_decoder.h>

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

#include "atomvm_esp_adf_audio_element.h"
#include "atomvm_esp_adf_common.h"

// #define ENABLE_TRACE
#include <trace.h>

#define MODULE_PREFIX "esp_adf_ogg_decoder:"
#define TAG "esp_adf_ogg_decoder"

static term nif_init(Context *ctx, int argc, term argv[])
{
    TRACE("%s\n", __func__);
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_list);
    term active_term = interop_kv_get_value_default(argv[0], ATOM_STR("\x6", "active"), TRUE_ATOM, ctx->global);

    if (UNLIKELY(memory_ensure_free(ctx, AUDIO_ELEMENT_OPAQUE_TERM_SIZE) != MEMORY_GC_OK)) {
        ESP_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    struct AudioElementResource *rsrc_obj = enif_alloc_resource(audio_element_resource_type, sizeof(struct AudioElementResource));
    if (IS_NULL_PTR(rsrc_obj)) {
        ESP_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    ogg_decoder_cfg_t ogg_cfg = DEFAULT_OGG_DECODER_CONFIG();
    if (UNLIKELY(!get_integer_parameter(ctx, argv, ATOM_STR("\xB", "out_rb_size"), &ogg_cfg.out_rb_size, false))) {
        return term_invalid_term();
    }
    if (UNLIKELY(!get_integer_parameter(ctx, argv, ATOM_STR("\xA", "task_stack"), &ogg_cfg.task_stack, false))) {
        return term_invalid_term();
    }
    if (UNLIKELY(!get_integer_parameter(ctx, argv, ATOM_STR("\x9", "task_core"), &ogg_cfg.task_core, false))) {
        return term_invalid_term();
    }
    if (UNLIKELY(!get_integer_parameter(ctx, argv, ATOM_STR("\x9", "task_prio"), &ogg_cfg.task_prio, false))) {
        return term_invalid_term();
    }
    if (UNLIKELY(!get_bool_parameter(ctx, argv, ATOM_STR("\xC", "stack_in_ext"), &ogg_cfg.stack_in_ext))) {
        return term_invalid_term();
    }
    audio_element_handle_t audio_element = ogg_decoder_init(&ogg_cfg);

    atomvm_esp_adf_audio_element_init_resource(rsrc_obj, audio_element, active_term == TRUE_ATOM, ctx);
    return atomvm_esp_adf_audio_element_resource_to_opaque(rsrc_obj, ctx);
}

static const struct Nif init_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_init
};

//
// Component Nif Entrypoints
//

static const struct Nif *get_nif(const char *nifname)
{
    if (memcmp(nifname, MODULE_PREFIX, strlen(MODULE_PREFIX))) {
        return NULL;
    }
    if (strcmp(nifname + strlen(MODULE_PREFIX), "init/1") == 0) {
        return &init_nif;
    }
    return NULL;
}

REGISTER_NIF_COLLECTION(esp_adf_ogg_decoder, NULL, NULL, get_nif)

#endif
