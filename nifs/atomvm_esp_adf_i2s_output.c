/* SPDX-License-Identifier: MIT */
#include <sdkconfig.h>

#ifdef CONFIG_AVM_ESP_ADF_I2S_OUTPUT_ENABLE

#include <stdlib.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"

#include <driver/gpio.h>
#include <driver/i2s_common.h>
#include <driver/i2s_std.h>
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
#include <term.h>

#include "atomvm_esp_adf_audio_element.h"

// #define ENABLE_TRACE
#include <trace.h>

#define MODULE_PREFIX "esp_adf_i2s_output:"
#define TAG "esp_adf_i2s_output"

struct I2SData
{
    i2s_chan_handle_t tx_handle;
    i2s_chan_config_t chan_cfg;
    i2s_std_config_t std_config;
};

static esp_err_t i2s_open(audio_element_handle_t self)
{
    TRACE("%s\n", __func__);

    struct I2SData *i2s = (struct I2SData *) audio_element_getdata(self);

    i2s_channel_enable(i2s->tx_handle);

    return ESP_OK;
}

static esp_err_t i2s_close(audio_element_handle_t self)
{
    TRACE("%s\n", __func__);

    struct I2SData *i2s = (struct I2SData *) audio_element_getdata(self);

    i2s_channel_disable(i2s->tx_handle);

    return ESP_OK;
}

static esp_err_t i2s_destroy(audio_element_handle_t self)
{
    TRACE("%s\n", __func__);

    struct I2SData *i2s = (struct I2SData *) audio_element_getdata(self);

    i2s_del_channel(i2s->tx_handle);

    free(i2s);

    return ESP_OK;
}

static int i2s_process(audio_element_handle_t self, char *buffer, int len)
{
    TRACE("%s\n", __func__);

    int process_len;
    int r_size = audio_element_input(self, buffer, len);

    if (r_size > 0) {
        audio_element_multi_output(self, buffer, r_size, 0);
        process_len = audio_element_output(self, buffer, r_size);
        audio_element_update_byte_pos(self, process_len);
    } else {
        process_len = r_size;
    }
    return process_len;
}

static int i2s_write(audio_element_handle_t self, char *buffer, int len, TickType_t ticks_to_wait, void *context)
{
    TRACE("%s\n", __func__);
    UNUSED(context);

    struct I2SData *i2s = (struct I2SData *) audio_element_getdata(self);

    size_t bytes_written = 0;

    i2s_channel_write(i2s->tx_handle, buffer, len, &bytes_written, ticks_to_wait);

    return bytes_written;
}

static void i2s_data_init(struct I2SData *driver_data, int rate, int gpio_mclk, int gpio_bclk, int gpio_lrclk, int gpio_dout)
{
    TRACE("%s\n", __func__);

    driver_data->chan_cfg = (i2s_chan_config_t) I2S_CHANNEL_DEFAULT_CONFIG(I2S_NUM_AUTO, I2S_ROLE_MASTER);
    i2s_new_channel(&driver_data->chan_cfg, &driver_data->tx_handle, NULL);

    driver_data->std_config = (i2s_std_config_t){
        .clk_cfg = I2S_STD_CLK_DEFAULT_CONFIG(rate),
        .slot_cfg = I2S_STD_MSB_SLOT_DEFAULT_CONFIG(I2S_DATA_BIT_WIDTH_32BIT, I2S_SLOT_MODE_STEREO),
        .gpio_cfg = {
            .mclk = gpio_mclk,
            .bclk = gpio_bclk,
            .ws = gpio_lrclk,
            .dout = gpio_dout,
            .din = I2S_GPIO_UNUSED,
            .invert_flags = {
                .mclk_inv = false,
                .bclk_inv = false,
                .ws_inv = false,
            },
        },
    };
    i2s_channel_init_std_mode(driver_data->tx_handle, &driver_data->std_config);
}

static term i2s_output_new(Context *ctx, term argv[], int rate, int gpio_mclk, int gpio_bclk, int gpio_lrclk, int gpio_dout)
{
    TRACE("%s\n", __func__);

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

    audio_element_cfg_t cfg = DEFAULT_AUDIO_ELEMENT_CONFIG();
    cfg.open = i2s_open;
    cfg.close = i2s_close;
    cfg.destroy = i2s_destroy;
    cfg.process = i2s_process;
    cfg.write = i2s_write;
    cfg.tag = "i2s";

    struct I2SData *driver_data = malloc(sizeof(struct I2SData));
    if (IS_NULL_PTR(driver_data)) {
        ESP_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    i2s_data_init(driver_data, rate, gpio_mclk, gpio_bclk, gpio_lrclk, gpio_dout);

    rsrc_obj->audio_element = audio_element_init(&cfg);
    if (IS_NULL_PTR(rsrc_obj->audio_element)) {
        ESP_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        free(driver_data);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    audio_element_setdata(rsrc_obj->audio_element, driver_data);

    return obj;
}

static term nif_init(Context *ctx, int argc, term argv[])
{
    TRACE("%s\n", __func__);
    UNUSED(argc);

    term cfg = argv[0];

    VALIDATE_VALUE(cfg, term_is_list);

    term rate_term = interop_kv_get_value_default(cfg, ATOM_STR("\x4", "rate"), term_from_int(48000), ctx->global);
    term gpio_mclk_term = interop_kv_get_value_default(cfg, ATOM_STR("\x9", "gpio_mclk"), term_from_int(I2S_GPIO_UNUSED), ctx->global);
    term gpio_bclk_term = interop_kv_get_value(cfg, ATOM_STR("\x9", "gpio_bclk"), ctx->global);
    term gpio_lrclk_term = interop_kv_get_value(cfg, ATOM_STR("\xA", "gpio_lrclk"), ctx->global);
    term gpio_dout_term = interop_kv_get_value(cfg, ATOM_STR("\x9", "gpio_dout"), ctx->global);

    VALIDATE_VALUE(rate_term, term_is_integer);
    VALIDATE_VALUE(gpio_mclk_term, term_is_integer);
    VALIDATE_VALUE(gpio_bclk_term, term_is_integer);
    VALIDATE_VALUE(gpio_lrclk_term, term_is_integer);
    VALIDATE_VALUE(gpio_dout_term, term_is_integer);

    avm_int_t rate = term_to_int(rate_term);
    avm_int_t gpio_mclk = term_to_int(gpio_mclk_term);
    avm_int_t gpio_bclk = term_to_int(gpio_bclk_term);
    avm_int_t gpio_lrclk = term_to_int(gpio_lrclk_term);
    avm_int_t gpio_dout = term_to_int(gpio_dout_term);

    if (UNLIKELY(gpio_mclk != I2S_GPIO_UNUSED && !GPIO_IS_VALID_GPIO(gpio_mclk))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (UNLIKELY(!GPIO_IS_VALID_GPIO(gpio_bclk))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (UNLIKELY(!GPIO_IS_VALID_GPIO(gpio_lrclk))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (UNLIKELY(!GPIO_IS_VALID_GPIO(gpio_dout))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    return i2s_output_new(ctx, argv, rate, gpio_mclk, gpio_bclk, gpio_lrclk, gpio_dout);
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

REGISTER_NIF_COLLECTION(esp_adf_i2s_output, NULL, NULL, get_nif)

#endif
