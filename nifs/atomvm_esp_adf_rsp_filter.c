/* SPDX-License-Identifier: MIT */
#include <sdkconfig.h>

#ifdef CONFIG_AVM_ESP_ADF_RSP_FILTER_ENABLE

#include <stdlib.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"

#include <esp_attr.h>
#include <esp_log.h>
#include <esp_system.h>

#include <esp_resample.h>
#include <filter_resample.h>

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

// #define ENABLE_TRACE
#include <trace.h>

#define MODULE_PREFIX "esp_adf_rsp_filter:"
#define TAG "esp_adf_rsp_filter"

static const AtomStringIntPair esp_resample_modes[] = {
    { ATOM_STR("\x6", "decode"), RESAMPLE_DECODE_MODE },
    { ATOM_STR("\x6", "encode"), RESAMPLE_ENCODE_MODE },
    { ATOM_STR("\x7", "uncross"), RESAMPLE_UNCROSS_MODE },
    SELECT_INT_DEFAULT(-1)
};

static const AtomStringIntPair esp_resample_types[] = {
    { ATOM_STR("\x4", "auto"), ESP_RESAMPLE_TYPE_AUTO },
    { ATOM_STR("\x8", "decimate"), ESP_RESAMPLE_TYPE_DECIMATE },
    { ATOM_STR("\x6", "interp"), ESP_RESAMPLE_TYPE_INTERP },
    { ATOM_STR("\x8", "resample"), ESP_RESAMPLE_TYPE_RESAMPLE },
    { ATOM_STR("\x6", "bypass"), ESP_RESAMPLE_TYPE_BYPASS },
    SELECT_INT_DEFAULT(-1)
};

static const AtomStringIntPair esp_rsp_prefer_types[] = {
    { ATOM_STR("\x6", "memory"), ESP_RSP_PREFER_TYPE_MEMORY },
    { ATOM_STR("\x5", "speed"), ESP_RSP_PREFER_TYPE_SPEED },
    SELECT_INT_DEFAULT(-1)
};

static bool get_integer_parameter(Context *ctx, term argv[], AtomString key, int *value)
{
    term parameter_term = interop_kv_get_value(argv[0], key, ctx->global);
    if (term_is_invalid_term(parameter_term)) {
        return true;
    }
    if (UNLIKELY(!term_is_integer(parameter_term))) {
        RAISE_ERROR(BADARG_ATOM);
        return false;
    }
    *value = term_to_int(parameter_term);
    return true;
}

static bool get_bool_parameter(Context *ctx, term argv[], AtomString key, bool *value)
{
    term parameter_term = interop_kv_get_value(argv[0], key, ctx->global);
    if (term_is_invalid_term(parameter_term)) {
        return true;
    }
    if (UNLIKELY(!term_is_atom(parameter_term) || (parameter_term != TRUE_ATOM && parameter_term != FALSE_ATOM))) {
        RAISE_ERROR(BADARG_ATOM);
        return false;
    }
    *value = parameter_term == TRUE_ATOM;
    return true;
}

static bool get_enum_parameter(Context *ctx, term argv[], AtomString key, const AtomStringIntPair *table, int *value)
{
    term parameter_term = interop_kv_get_value(argv[0], key, ctx->global);
    if (term_is_invalid_term(parameter_term)) {
        return true;
    }
    if (UNLIKELY(!term_is_atom(parameter_term))) {
        RAISE_ERROR(BADARG_ATOM);
        return false;
    }
    int enum_value = interop_atom_term_select_int(table, parameter_term, ctx->global);
    if (enum_value < 0) {
        return false;
    }
    *value = enum_value;
    return true;
}

static term nif_init(Context *ctx, int argc, term argv[])
{
    TRACE("%s\n", __func__);
    UNUSED(argc);

    term cfg = argv[0];

    VALIDATE_VALUE(cfg, term_is_list);
    term active_term = interop_kv_get_value_default(argv[0], ATOM_STR("\x6", "active"), TRUE_ATOM, ctx->global);

    rsp_filter_cfg_t filter_cfg = DEFAULT_RESAMPLE_FILTER_CONFIG();

    if (UNLIKELY(!get_integer_parameter(ctx, argv, ATOM_STR("\x8", "src_bits"), &filter_cfg.src_bits))) {
        return term_invalid_term();
    }
    if (UNLIKELY(!get_integer_parameter(ctx, argv, ATOM_STR("\x6", "src_ch"), &filter_cfg.src_ch))) {
        return term_invalid_term();
    }
    if (UNLIKELY(!get_integer_parameter(ctx, argv, ATOM_STR("\x8", "src_rate"), &filter_cfg.src_rate))) {
        return term_invalid_term();
    }
    if (UNLIKELY(!get_integer_parameter(ctx, argv, ATOM_STR("\x9", "dest_bits"), &filter_cfg.dest_bits))) {
        return term_invalid_term();
    }
    if (UNLIKELY(!get_integer_parameter(ctx, argv, ATOM_STR("\x7", "dest_ch"), &filter_cfg.dest_ch))) {
        return term_invalid_term();
    }
    if (UNLIKELY(!get_integer_parameter(ctx, argv, ATOM_STR("\x9", "dest_rate"), &filter_cfg.dest_rate))) {
        return term_invalid_term();
    }
    if (UNLIKELY(!get_enum_parameter(ctx, argv, ATOM_STR("\x4", "mode"), esp_resample_modes, (int *) &filter_cfg.mode))) {
        return term_invalid_term();
    }
    if (UNLIKELY(!get_integer_parameter(ctx, argv, ATOM_STR("\x10", "max_indata_bytes"), &filter_cfg.max_indata_bytes))) {
        return term_invalid_term();
    }
    if (UNLIKELY(!get_integer_parameter(ctx, argv, ATOM_STR("\xD", "out_len_bytes"), &filter_cfg.out_len_bytes))) {
        return term_invalid_term();
    }
    if (UNLIKELY(!get_enum_parameter(ctx, argv, ATOM_STR("\x4", "type"), esp_resample_types, &filter_cfg.type))) {
        return term_invalid_term();
    }
    if (UNLIKELY(!get_integer_parameter(ctx, argv, ATOM_STR("\xA", "complexity"), &filter_cfg.complexity))) {
        return term_invalid_term();
    }
    if (UNLIKELY(!get_integer_parameter(ctx, argv, ATOM_STR("\xB", "down_ch_idx"), &filter_cfg.down_ch_idx))) {
        return term_invalid_term();
    }
    if (UNLIKELY(!get_enum_parameter(ctx, argv, ATOM_STR("\xB", "prefer_flag"), esp_rsp_prefer_types, (int *) &filter_cfg.prefer_flag))) {
        return term_invalid_term();
    }
    if (UNLIKELY(!get_integer_parameter(ctx, argv, ATOM_STR("\xB", "out_rb_size"), &filter_cfg.out_rb_size))) {
        return term_invalid_term();
    }
    if (UNLIKELY(!get_integer_parameter(ctx, argv, ATOM_STR("\xA", "task_stack"), &filter_cfg.task_stack))) {
        return term_invalid_term();
    }
    if (UNLIKELY(!get_integer_parameter(ctx, argv, ATOM_STR("\x9", "task_core"), &filter_cfg.task_core))) {
        return term_invalid_term();
    }
    if (UNLIKELY(!get_integer_parameter(ctx, argv, ATOM_STR("\x9", "task_prio"), &filter_cfg.task_prio))) {
        return term_invalid_term();
    }
    if (UNLIKELY(!get_bool_parameter(ctx, argv, ATOM_STR("\xC", "stack_in_ext"), &filter_cfg.stack_in_ext))) {
        return term_invalid_term();
    }

    if (UNLIKELY(memory_ensure_free(ctx, AUDIO_ELEMENT_OPAQUE_TERM_SIZE) != MEMORY_GC_OK)) {
        ESP_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    struct AudioElementResource *rsrc_obj = enif_alloc_resource(audio_element_resource_type, sizeof(struct AudioElementResource));
    if (IS_NULL_PTR(rsrc_obj)) {
        ESP_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    audio_element_handle_t audio_element = rsp_filter_init(&filter_cfg);
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

REGISTER_NIF_COLLECTION(esp_adf_rsp_filter, NULL, NULL, get_nif)

#endif
