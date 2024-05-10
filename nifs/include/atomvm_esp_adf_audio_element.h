/* SPDX-License-Identifier: MIT */
#include <sdkconfig.h>

#include <audio_element.h>

#include <erl_nif.h>
#include <esp32_sys.h>
#include <globalcontext.h>
#include <smp.h>

#ifdef CONFIG_AVM_ESP_ADF_AUDIO_ELEMENT_ENABLE

struct AudioElementResource
{
    audio_element_handle_t audio_element;
    uint64_t ref_ticks;
    struct EventListener event_listener;
    int32_t owner_process_id;
    ErlNifMonitor owner_process_monitor;
    bool active;
    int32_t trapped_process_id;
#ifndef AVM_NO_SMP
    Mutex *mutex;
#endif
};

extern ErlNifResourceType *audio_element_resource_type;

// Size including boxed resource
#define AUDIO_ELEMENT_OPAQUE_TERM_SIZE (TERM_BOXED_RESOURCE_SIZE + TUPLE_SIZE(2) + REF_SIZE)

/**
 * @brief Initialize resource for a given audio element.
 * @details Initialize resource and event listener for the audio element.
 * Also creates the reference tick to be used for events, sets the owner
 * process and create a monitor. If the audio element is active, increment
 * the reference counter as the resource will send events to the process.
 * @param resource the resource to initialize
 * @param audio_element the associated audio element
 * @param active if the process should actively receive events
 * @param ctx the owner context
 */
void atomvm_esp_adf_audio_element_init_resource(struct AudioElementResource *resource, audio_element_handle_t audio_element, bool active, Context *ctx);

/**
 * @brief Extract the resource object from audio_element opaque term.
 * @param opaque reference to the audio element
 * @param ctx current context
 * @return the resource or \c NULL if opaque is not a resource
 */
struct AudioElementResource *atomvm_esp_adf_audio_element_opaque_to_resource(term opaque, Context *ctx);

/**
 * @brief Build an opaque term from a resource object
 * @details Provided heap should have at least AUDIO_ELEMENT_OPAQUE_TERM_SIZE free.
 * @param resource resource to embed
 * @param ctx context whose heap to allocate the opaque term to
 * @return an opaque term embedding the object
 */
term atomvm_esp_adf_audio_element_resource_to_opaque(struct AudioElementResource *resource, Context *ctx);

#endif
