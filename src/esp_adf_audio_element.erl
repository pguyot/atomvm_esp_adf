% SPDX-License-Identifier: MIT
-module(esp_adf_audio_element).

-export_type([
    audio_element/0,
    audio_element_opts/0,
    audio_element_opt/0,
    audio_element_status/0,
    audio_element_codec_fmt/0,
    audio_element_position_prop/0,
    audio_element_event/0
]).

-export([
    set_read_binary/2,
    setopts/2,
    controlling_process/2,
    get_event/1,
    get_event/2
]).

% Resource for audio elements
-opaque audio_element() :: {reference(), binary()}.

% Options
-type audio_element_opts() :: [audio_element_opt()].
-type audio_element_opt() ::
    {active, boolean()}.

% Events
-type audio_element_status() ::
    none
    | error_open
    | error_input
    | error_process
    | error_output
    | error_close
    | error_timeout
    | error_unknown
    | input_done
    | input_buffering
    | output_done
    | output_buffering
    | state_running
    | state_paused
    | state_stopped
    | state_finished
    | mounted
    | unmounted.

-type audio_element_codec_fmt() ::
    unknown
    | raw
    | wav
    | mp3
    | aac
    | opus
    | m4a
    | mp4
    | flac
    | ogg
    | tsaac
    | amrnb
    | amrwb
    | pcm
    | m3u8
    | pls
    | unsupported.

-type audio_element_position_prop() ::
    {sample_rates, pos_integer()}
    | {channels, pos_integer()}
    | {bits, pos_integer()}
    | {bps, pos_integer()}
    | {byte_pos, integer()}
    | {total_bytes, integer()}
    | {duration, pos_integer()}
    | {codec_fmt, audio_element_codec_fmt()}.

-type audio_element_event() ::
    {status, audio_element_status()}
    | music_info
    | codec_fmt
    | {position, [audio_element_position_prop()]}.

%% @doc Set read callback to read a given binary
%% If binary is reference counted, it is not copied.
%% @param AudioElement element to set read callback to
%% @param Binary binary to read
%% @return ok
-spec set_read_binary(AudioElement :: audio_element(), Binary :: binary()) -> ok.
set_read_binary(_AudioElement, _Binary) ->
    erlang:nif_error(undefined).

%% @doc Set options to the audio element.
%% The only available option is `active'. It defaults to true, which means that
%% events will be sent to the owner process. Events are sent as tuples:
%% `{audio_element, audio_element(), audio_element_event()}'
%% @param AudioElement element to set options
%% @param Options to set
%% @return ok
-spec setopts(AudioElement :: audio_element(), Options :: audio_element_opts()) -> ok.
setopts(_AudioElement, _Options) ->
    erlang:nif_error(undefined).

%% @doc Set the controlling process, i.e. the process that receive event
%% messages from the audio element. If called by another process, returns
%% `{error, not_owner}'. If `Pid' does not refer to a valid process, returns
%% `{error, badarg}'.
%% @param AudioElement element to change the controlling process of
%% @param Pid new controlling process
%% @return ok or an error.
-spec controlling_process(AudioElement :: audio_element(), Pid :: pid()) ->
    ok | {error, not_owner | badarg}.
controlling_process(_AudioElement, _Pid) ->
    erlang:nif_error(undefined).

%% @doc Get an event synchronously.
%% This function can be called on an `audio_element()' that is configured in
%% passive mode.
%% @equiv get_event(AudioElement, infinity)
%% @param AudioElement element to read an event from.
%% @return the next available event.
-spec get_event(AudioElement :: audio_element()) -> {ok, audio_element_event()}.
get_event(_AudioElement) ->
    erlang:nif_error(undefined).

%% @doc Get an event synchronously.
%% This function can be called on an `audio_element()' that is configured in
%% passive mode.
%% @param AudioElement element to read an event from.
%% @param Timeout timeout to wait for the event. Can only be 0 or infinity.
%% @return the next available event or an error if no event occurred before timeout.
-spec get_event(AudioElement :: audio_element(), Timeout :: 0 | infinity) ->
    {ok, audio_element_event()} | {error, timeout}.
get_event(_AudioElement, _Timeout) ->
    erlang:nif_error(undefined).
