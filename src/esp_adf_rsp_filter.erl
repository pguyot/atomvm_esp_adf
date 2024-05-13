% SPDX-License-Identifier: MIT
-module(esp_adf_rsp_filter).

-export([init/1]).

-type esp_resample_mode() :: decode | encode | uncross.

-type esp_resample_type() :: auto | decimate | interp | resample | bypass.

-type esp_rsp_prefer_type() :: memory | speed.

-type rsp_filter_option() ::
    esp_adf_audio_element:audio_element_opt()
    | {src_bits, 8 | 16 | 24 | 32}
    | {src_ch, 1 | 2}
    | {src_rate, pos_integer()}
    | {dest_bits, 16}
    | {dest_ch, 1 | 2}
    | {dest_rate, pos_integer()}
    | {mode, esp_resample_mode()}
    | {max_indata_bytes, pos_integer()}
    | {out_len_bytes, pos_integer()}
    | {type, esp_resample_type()}
    | {complexity, 0 | 1..5}
    | {down_ch_idx, non_neg_integer()}
    | {prefer_flag, esp_rsp_prefer_type()}
    | {out_rb_size, pos_integer()}
    | {task_stack, pos_integer()}
    | {task_core, non_neg_integer()}
    | {task_prio, non_neg_integer()}
    | {stack_in_ext, boolean()}.

%% @doc Create a handle to an MP3 decoder audio element.
%% @param Cfg configuration
%% @return an audio element resource
-spec init([rsp_filter_option()]) -> esp_adf_audio_element:audio_element().
init(_Cfg) ->
    erlang:nif_error(undefined).
