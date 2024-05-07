/* SPDX-License-Identifier: MIT */
#include <sdkconfig.h>

#include <audio_element.h>

#include <erl_nif.h>

#ifdef CONFIG_AVM_ESP_ADF_AUDIO_ELEMENT_ENABLE

struct AudioElementResource
{
    audio_element_handle_t audio_element;
};

extern ErlNifResourceType *audio_element_resource_type;

#endif
