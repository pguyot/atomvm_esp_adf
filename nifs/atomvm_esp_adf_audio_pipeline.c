/* SPDX-License-Identifier: MIT */
#include <sdkconfig.h>

#ifdef CONFIG_AVM_ESP_ADF_AUDIO_PIPELINE_ENABLE

#include <stdlib.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"

#include <audio_pipeline.h>
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

#define MODULE_PREFIX "esp_adf_audio_pipeline:"
#define TAG "esp_adf_audio_pipeline"

//
// Resources
//

struct AudioPipelineResource
{
    audio_pipeline_handle_t audio_pipeline;
};

static void audio_pipeline_dtor(ErlNifEnv *caller_env, void *obj)
{
    TRACE("%s:%s\n", __FILE__, __func__);
    UNUSED(caller_env);

    struct AudioPipelineResource *rsrc_obj = (struct AudioPipelineResource *) obj;
    audio_pipeline_deinit(rsrc_obj->audio_pipeline);
}

static const ErlNifResourceTypeInit audio_pipeline_resource_type_init = {
    .members = 1,
    .dtor = audio_pipeline_dtor,
};

static ErlNifResourceType *audio_pipeline_resource_type;

//
// Nifs
//

static term nif_init(Context *ctx, int argc, term argv[])
{
    TRACE("%s:%s\n", __FILE__, __func__);
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_list);

    if (UNLIKELY(memory_ensure_free(ctx, TERM_BOXED_RESOURCE_SIZE) != MEMORY_GC_OK)) {
        ESP_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    struct AudioPipelineResource *rsrc_obj = enif_alloc_resource(audio_pipeline_resource_type, sizeof(struct AudioPipelineResource));
    if (IS_NULL_PTR(rsrc_obj)) {
        ESP_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term obj = enif_make_resource(erl_nif_env_from_context(ctx), rsrc_obj);
    enif_release_resource(rsrc_obj);

    audio_pipeline_cfg_t pipeline_cfg = DEFAULT_AUDIO_PIPELINE_CONFIG();
    rsrc_obj->audio_pipeline = audio_pipeline_init(&pipeline_cfg);

    return obj;
}

static term nif_register(Context *ctx, int argc, term argv[])
{
    TRACE("%s:%s\n", __FILE__, __func__);
    UNUSED(argc);

    void *pipeline_rsrc_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], audio_pipeline_resource_type, &pipeline_rsrc_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct AudioPipelineResource *audio_pipeline_obj = (struct AudioPipelineResource *) pipeline_rsrc_obj_ptr;

    struct AudioElementResource *audio_element_obj = atomvm_esp_adf_audio_element_opaque_to_resource(argv[1], ctx);
    if (IS_NULL_PTR(audio_element_obj)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    int ok;
    char *name = interop_term_to_string(argv[2], &ok);
    if (UNLIKELY(!ok)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    esp_err_t err = audio_pipeline_register(audio_pipeline_obj->audio_pipeline, audio_element_obj->audio_element, name);
    free(name);

    if (UNLIKELY(err != ESP_OK)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    // Avoid crashes if pipeline is freed before registered element (users must
    // call unregister).
    enif_keep_resource(audio_element_obj);

    return OK_ATOM;
}

static term nif_unregister(Context *ctx, int argc, term argv[])
{
    TRACE("%s:%s\n", __FILE__, __func__);
    UNUSED(argc);

    void *pipeline_rsrc_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], audio_pipeline_resource_type, &pipeline_rsrc_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct AudioPipelineResource *audio_pipeline_obj = (struct AudioPipelineResource *) pipeline_rsrc_obj_ptr;

    struct AudioElementResource *audio_element_obj = atomvm_esp_adf_audio_element_opaque_to_resource(argv[1], ctx);
    if (IS_NULL_PTR(audio_element_obj)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    esp_err_t err = audio_pipeline_unregister(audio_pipeline_obj->audio_pipeline, audio_element_obj->audio_element);

    if (UNLIKELY(err != ESP_OK)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    // Audio element can be freed now.
    enif_release_resource(audio_element_obj);

    return OK_ATOM;
}

static term nif_link(Context *ctx, int argc, term argv[])
{
    TRACE("%s:%s\n", __FILE__, __func__);
    UNUSED(argc);

    VALIDATE_VALUE(argv[1], term_is_list);

    int proper = 0;
    size_t nb_elems = term_list_length(argv[1], &proper);
    if (UNLIKELY(!proper)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (UNLIKELY(nb_elems == 0)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    char *names[nb_elems];

    int ix;
    term list = argv[1];
    for (ix = 0; ix < nb_elems; ix++) {
        term name = term_get_list_head(list);
        list = term_get_list_tail(list);
        int ok = 1;
        names[ix] = interop_term_to_string(name, &ok);
        if (UNLIKELY(!ok)) {
            for (ix--; ix >= 0; ix--) {
                free(names[ix]);
            }
            RAISE_ERROR(BADARG_ATOM);
        }
    }

    void *pipeline_rsrc_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], audio_pipeline_resource_type, &pipeline_rsrc_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct AudioPipelineResource *audio_pipeline_obj = (struct AudioPipelineResource *) pipeline_rsrc_obj_ptr;

    esp_err_t err = audio_pipeline_link(audio_pipeline_obj->audio_pipeline, (const char **) names, nb_elems);
    for (ix = 0; ix < nb_elems; ix++) {
        free(names[ix]);
    }

    if (UNLIKELY(err != ESP_OK)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    return OK_ATOM;
}

static term nif_run(Context *ctx, int argc, term argv[])
{
    TRACE("%s:%s\n", __FILE__, __func__);
    UNUSED(argc);

    void *pipeline_rsrc_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], audio_pipeline_resource_type, &pipeline_rsrc_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct AudioPipelineResource *audio_pipeline_obj = (struct AudioPipelineResource *) pipeline_rsrc_obj_ptr;

    esp_err_t err = audio_pipeline_run(audio_pipeline_obj->audio_pipeline);

    if (UNLIKELY(err != ESP_OK)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    // Avoid crashes if pipeline is freed before being terminated
    enif_keep_resource(audio_pipeline_obj);

    return OK_ATOM;
}

static term nif_stop(Context *ctx, int argc, term argv[])
{
    TRACE("%s:%s\n", __FILE__, __func__);
    UNUSED(argc);

    void *pipeline_rsrc_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], audio_pipeline_resource_type, &pipeline_rsrc_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct AudioPipelineResource *audio_pipeline_obj = (struct AudioPipelineResource *) pipeline_rsrc_obj_ptr;

    esp_err_t err = audio_pipeline_stop(audio_pipeline_obj->audio_pipeline);

    if (UNLIKELY(err != ESP_OK)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    return OK_ATOM;
}

// This is a dirty nif
static term nif_wait_for_stop(Context *ctx, int argc, term argv[])
{
    TRACE("%s:%s\n", __FILE__, __func__);
    UNUSED(argc);

    void *pipeline_rsrc_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], audio_pipeline_resource_type, &pipeline_rsrc_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct AudioPipelineResource *audio_pipeline_obj = (struct AudioPipelineResource *) pipeline_rsrc_obj_ptr;

    esp_err_t err = audio_pipeline_wait_for_stop(audio_pipeline_obj->audio_pipeline);

    if (UNLIKELY(err != ESP_OK)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    return OK_ATOM;
}

static term nif_terminate(Context *ctx, int argc, term argv[])
{
    TRACE("%s:%s\n", __FILE__, __func__);
    UNUSED(argc);

    void *pipeline_rsrc_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], audio_pipeline_resource_type, &pipeline_rsrc_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct AudioPipelineResource *audio_pipeline_obj = (struct AudioPipelineResource *) pipeline_rsrc_obj_ptr;

    esp_err_t err = audio_pipeline_terminate(audio_pipeline_obj->audio_pipeline);

    if (UNLIKELY(err != ESP_OK)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    // Audio pipeline can be freed now.
    enif_release_resource(audio_pipeline_obj);

    return OK_ATOM;
}

static const struct Nif init_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_init
};
static const struct Nif register_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_register
};
static const struct Nif unregister_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_unregister
};
static const struct Nif link_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_link
};
static const struct Nif run_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_run
};
static const struct Nif stop_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_stop
};
static const struct Nif wait_for_stop_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_wait_for_stop
};
static const struct Nif terminate_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_terminate
};

//
// Component Nif Entrypoints
//

static const struct Nif *get_nif(const char *nifname)
{
    TRACE("%s(nifname = %s)\n", __func__, nifname);

    if (memcmp(nifname, MODULE_PREFIX, strlen(MODULE_PREFIX))) {
        return NULL;
    }
    if (strcmp(nifname + strlen(MODULE_PREFIX), "init/1") == 0) {
        return &init_nif;
    }
    if (strcmp(nifname + strlen(MODULE_PREFIX), "register/3") == 0) {
        return &register_nif;
    }
    if (strcmp(nifname + strlen(MODULE_PREFIX), "unregister/2") == 0) {
        return &unregister_nif;
    }
    if (strcmp(nifname + strlen(MODULE_PREFIX), "link/2") == 0) {
        return &link_nif;
    }
    if (strcmp(nifname + strlen(MODULE_PREFIX), "run/1") == 0) {
        return &run_nif;
    }
    if (strcmp(nifname + strlen(MODULE_PREFIX), "stop/1") == 0) {
        return &stop_nif;
    }
    if (strcmp(nifname + strlen(MODULE_PREFIX), "wait_for_stop/1") == 0) {
        return &wait_for_stop_nif;
    }
    if (strcmp(nifname + strlen(MODULE_PREFIX), "terminate/1") == 0) {
        return &terminate_nif;
    }
    return NULL;
}

static void esp_adf_audio_pipeline_init(GlobalContext *global)
{
    TRACE("%s:%s\n", __FILE__, __func__);

    ErlNifEnv env;
    erl_nif_env_partial_init_from_globalcontext(&env, global);
    audio_pipeline_resource_type = enif_init_resource_type(&env, "audio_pipeline", &audio_pipeline_resource_type_init, ERL_NIF_RT_CREATE, NULL);
}

REGISTER_NIF_COLLECTION(esp_adf_audio_pipeline, esp_adf_audio_pipeline_init, NULL, get_nif)

#endif
