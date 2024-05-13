% SPDX-License-Identifier: MIT
-module(esp_adf_mp3_decoder).

-export([init/1]).

-type mp3_decoder_option() ::
    esp_adf_audio_element:audio_element_opt()
    | {out_rb_size, pos_integer()}
    | {task_stack, pos_integer()}
    | {task_core, non_neg_integer()}
    | {task_prio, non_neg_integer()}
    | {stack_in_ext, boolean()}
    | {id3_parse_enable, boolean()}.

%% @doc Create a handle to an MP3 decoder audio element.
%% @param Cfg configuration
%% @return an audio element resource
-spec init([mp3_decoder_option()]) -> esp_adf_audio_element:audio_element().
init(_Cfg) ->
    erlang:nif_error(undefined).
