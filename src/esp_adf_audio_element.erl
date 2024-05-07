% SPDX-License-Identifier: MIT
-module(esp_adf_audio_element).

-export_type([audio_element/0]).
-export([set_read_binary/2]).

% Resource for audio elements
-opaque audio_element() :: binary().

%% @doc Set read callback to read a given binary
%% If binary is reference counted, it is not copied.
%% @param _AudioElement element to set read callback to
%% @param _Binary binary to read
%% @return ok
-spec set_read_binary(audio_element(), binary()) -> ok.
set_read_binary(_AudioElement, _Binary) ->
    erlang:nif_error(undefined).
