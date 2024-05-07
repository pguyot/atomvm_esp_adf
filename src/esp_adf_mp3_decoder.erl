% SPDX-License-Identifier: MIT
-module(esp_adf_mp3_decoder).

-export([init/1]).

%% @doc Create a handle to an MP3 decoder audio element.
%% @param Cfg configuration
%% @return an audio element resource
-spec init([]) -> audio_element:audio_element().
init(_Cfg) ->
    erlang:nif_error(undefined).
