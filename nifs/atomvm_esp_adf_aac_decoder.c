/* SPDX-License-Identifier: MIT */
#include <sdkconfig.h>

#ifdef CONFIG_AVM_ESP_ADF_AAC_DECODER_ENABLE

#include <stdlib.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"

#include <aac_decoder.h>
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

#include "atomvm_esp_adf_audio_element.h"

// #define ENABLE_TRACE
#include <trace.h>

#define MODULE_PREFIX "esp_adf_aac_decoder:"
#define TAG "esp_adf_aac_decoder"

static term nif_init(Context *ctx, int argc, term argv[])
{
    TRACE("%s\n", __func__);
    UNUSED(argc);

    VALIDATE_VALUE(argv[1], term_is_list);

    if (UNLIKELY(memory_ensure_free(ctx, TERM_BOXED_RESOURCE_SIZE) != MEMORY_GC_OK)) {
        ESP_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    struct AudioElementResource *rsrc_obj = enif_alloc_resource(audio_element_resource_type, sizeof(struct AudioElementResource));
    if (IS_NULL_PTR(rsrc_obj)) {
        ESP_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term obj = enif_make_resource(erl_nif_env_from_context(ctx), rsrc_obj);
    enif_release_resource(rsrc_obj);

    aac_decoder_cfg_t aac_cfg = DEFAULT_AAC_DECODER_CONFIG();
    rsrc_obj->audio_element = aac_decoder_init(&aac_cfg);

    return obj;
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

REGISTER_NIF_COLLECTION(esp_adf_aac_decoder, NULL, NULL, get_nif)

#endif
