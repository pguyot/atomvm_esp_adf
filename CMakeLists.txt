#
# This file is part of AtomVM.
#
# Copyright 2024 Paul Guyot <pguyot@kallisys.net>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#

set(ATOMVM_ESP_AUDIO_COMPONENT_SRCS
    "nifs/atomvm_esp_adf_aac_decoder.c"
    "nifs/atomvm_esp_adf_amr_decoder.c"
    "nifs/atomvm_esp_adf_audio_element.c"
    "nifs/atomvm_esp_adf_audio_pipeline.c"
    "nifs/atomvm_esp_adf_common.c"
    "nifs/atomvm_esp_adf_flac_decoder.c"
    "nifs/atomvm_esp_adf_g711_decoder.c"
    "nifs/atomvm_esp_adf_i2s_output.c"
    "nifs/atomvm_esp_adf_mp3_decoder.c"
    "nifs/atomvm_esp_adf_ogg_decoder.c"
    "nifs/atomvm_esp_adf_opus_decoder.c"
    "nifs/atomvm_esp_adf_pcm_decoder.c"
    "nifs/atomvm_esp_adf_rsp_filter.c"
    "nifs/atomvm_esp_adf_wav_decoder.c"
)

# WHOLE_ARCHIVE option is supported only with esp-idf 5.x
# A link option will be used with esp-idf 4.x
if (IDF_VERSION_MAJOR EQUAL 5)
    set(OPTIONAL_WHOLE_ARCHIVE WHOLE_ARCHIVE)
else()
    set(OPTIONAL_WHOLE_ARCHIVE "")
endif()

idf_component_register(
  SRCS ${ATOMVM_ESP_AUDIO_COMPONENT_SRCS}
  INCLUDE_DIRS "nifs/include"
  PRIV_REQUIRES "libatomvm" "avm_sys" "driver" "audio_pipeline"
  ${OPTIONAL_WHOLE_ARCHIVE}
)

if (IDF_VERSION_MAJOR EQUAL 4)
    idf_build_set_property(
        LINK_OPTIONS "-Wl,--whole-archive ${CMAKE_CURRENT_BINARY_DIR}/lib${COMPONENT_NAME}.a -Wl,--no-whole-archive"
        APPEND)
endif()
