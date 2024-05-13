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

#include <audio_element.h>

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
#include "atomvm_esp_adf_common.h"

// #define ENABLE_TRACE
#include <trace.h>

#define MODULE_PREFIX "esp_adf_i2s_output:"
#define TAG "esp_adf_i2s_output"

typedef struct
{
    bool active;
    int gpio_mclk;
    int gpio_bclk;
    int gpio_lrclk;
    int gpio_dout;
    int rate;
    int bits;
    int channels;
} i2s_output_cfg_t;

#define DEFAULT_I2S_OUTPUT_CONFIG()   \
    {                                 \
        .active = true,               \
        .gpio_mclk = I2S_GPIO_UNUSED, \
        .gpio_bclk = 0,               \
        .gpio_lrclk = 0,              \
        .gpio_dout = 0,               \
        .rate = 48000,                \
        .bits = 16,                   \
        .channels = 2                 \
    }

struct I2SData
{
    i2s_chan_handle_t tx_handle;
    i2s_chan_config_t chan_cfg;
    i2s_std_config_t std_config;
};

struct MusicInfo
{
    int rate;
    i2s_slot_mode_t channels;
    i2s_data_bit_width_t bits;
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

static void i2s_data_init(struct I2SData *driver_data, const i2s_output_cfg_t *cfg)
{
    TRACE("%s\n", __func__);

    driver_data->chan_cfg = (i2s_chan_config_t) I2S_CHANNEL_DEFAULT_CONFIG(I2S_NUM_AUTO, I2S_ROLE_MASTER);
    i2s_new_channel(&driver_data->chan_cfg, &driver_data->tx_handle, NULL);

    driver_data->std_config = (i2s_std_config_t){
        .clk_cfg = I2S_STD_CLK_DEFAULT_CONFIG(cfg->rate),
        .slot_cfg = I2S_STD_PHILIPS_SLOT_DEFAULT_CONFIG(cfg->bits, cfg->channels),
        .gpio_cfg = {
            .mclk = cfg->gpio_mclk,
            .bclk = cfg->gpio_bclk,
            .ws = cfg->gpio_lrclk,
            .dout = cfg->gpio_dout,
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

static term i2s_output_new(Context *ctx, term argv[], const i2s_output_cfg_t *cfg)
{
    TRACE("%s\n", __func__);

    if (UNLIKELY(memory_ensure_free(ctx, AUDIO_ELEMENT_OPAQUE_TERM_SIZE) != MEMORY_GC_OK)) {
        ESP_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    struct AudioElementResource *rsrc_obj = enif_alloc_resource(audio_element_resource_type, sizeof(struct AudioElementResource));
    if (IS_NULL_PTR(rsrc_obj)) {
        ESP_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    audio_element_cfg_t ae_cfg = DEFAULT_AUDIO_ELEMENT_CONFIG();
    ae_cfg.open = i2s_open;
    ae_cfg.close = i2s_close;
    ae_cfg.destroy = i2s_destroy;
    ae_cfg.process = i2s_process;
    ae_cfg.write = i2s_write;
    ae_cfg.tag = "i2s";

    struct I2SData *driver_data = malloc(sizeof(struct I2SData));
    if (IS_NULL_PTR(driver_data)) {
        ESP_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    i2s_data_init(driver_data, cfg);

    audio_element_handle_t audio_element = audio_element_init(&ae_cfg);
    if (IS_NULL_PTR(audio_element)) {
        ESP_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        free(driver_data);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    audio_element_setdata(audio_element, driver_data);
    audio_element_set_music_info(audio_element, cfg->rate, cfg->channels, cfg->bits);

    atomvm_esp_adf_audio_element_init_resource(rsrc_obj, audio_element, cfg->active, ctx);
    return atomvm_esp_adf_audio_element_resource_to_opaque(rsrc_obj, ctx);
}

static term nif_init(Context *ctx, int argc, term argv[])
{
    TRACE("%s\n", __func__);
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_list);

    i2s_output_cfg_t cfg = DEFAULT_I2S_OUTPUT_CONFIG();
    if (UNLIKELY(!get_bool_parameter(ctx, argv, ATOM_STR("\x6", "active"), &cfg.active))) {
        return term_invalid_term();
    }
    if (UNLIKELY(!get_integer_parameter(ctx, argv, ATOM_STR("\x9", "gpio_mclk"), &cfg.gpio_mclk, false))) {
        return term_invalid_term();
    }
    if (UNLIKELY(!get_integer_parameter(ctx, argv, ATOM_STR("\x9", "gpio_bclk"), &cfg.gpio_bclk, true))) {
        return term_invalid_term();
    }
    if (UNLIKELY(!get_integer_parameter(ctx, argv, ATOM_STR("\xA", "gpio_lrclk"), &cfg.gpio_lrclk, true))) {
        return term_invalid_term();
    }
    if (UNLIKELY(!get_integer_parameter(ctx, argv, ATOM_STR("\x9", "gpio_dout"), &cfg.gpio_dout, true))) {
        return term_invalid_term();
    }
    if (UNLIKELY(!get_integer_parameter(ctx, argv, ATOM_STR("\x4", "rate"), &cfg.rate, false))) {
        return term_invalid_term();
    }
    if (UNLIKELY(!get_integer_parameter(ctx, argv, ATOM_STR("\x4", "bits"), &cfg.bits, false))) {
        return term_invalid_term();
    }
    if (UNLIKELY(!get_integer_parameter(ctx, argv, ATOM_STR("\x8", "channels"), &cfg.channels, false))) {
        return term_invalid_term();
    }

    if (UNLIKELY(cfg.gpio_mclk != I2S_GPIO_UNUSED && !GPIO_IS_VALID_GPIO(cfg.gpio_mclk))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (UNLIKELY(!GPIO_IS_VALID_GPIO(cfg.gpio_bclk))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (UNLIKELY(!GPIO_IS_VALID_GPIO(cfg.gpio_lrclk))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (UNLIKELY(!GPIO_IS_VALID_GPIO(cfg.gpio_dout))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    // In standard mode, there are always 2 channels
    if (UNLIKELY(cfg.channels != 2)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    // In standard mode, bits can be 8/16/24 or 32
    if (UNLIKELY(cfg.bits != 8 && cfg.bits != 16 && cfg.bits != 24 && cfg.bits != 32)) {
        RAISE_ERROR(BADARG_ATOM);
    }

#if __STDC_VERSION__ >= 201112L
    _Static_assert(I2S_SLOT_MODE_STEREO == 2, "expected I2S_SLOT_MODE_STEREO to be equal to 2");
    _Static_assert(I2S_DATA_BIT_WIDTH_8BIT == 8, "expected I2S_DATA_BIT_WIDTH_8BIT to be equal to 8");
    _Static_assert(I2S_DATA_BIT_WIDTH_16BIT == 16, "expected I2S_DATA_BIT_WIDTH_16BIT to be equal to 16");
    _Static_assert(I2S_DATA_BIT_WIDTH_24BIT == 24, "expected I2S_DATA_BIT_WIDTH_24BIT to be equal to 24");
    _Static_assert(I2S_DATA_BIT_WIDTH_32BIT == 32, "expected I2S_DATA_BIT_WIDTH_32BIT to be equal to 32");
#endif

    return i2s_output_new(ctx, argv, &cfg);
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
