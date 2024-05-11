%%%-------------------------------------------------------------------
%% @doc Example code to play a mono MP3 file over i2s
%% @end
%%%-------------------------------------------------------------------

-module(play_mp3_mono_i2s).

-export([start/0]).

%% GPIO configuration

-define(I2S_LRC_GPIO, 7).
-define(I2S_BCLK_GPIO, 6).
-define(I2S_DIN_GPIO, 5).

flush_messages() ->
    receive
        {audio_element, _AudioElement, _Message} = Msg ->
            io:format("~p\n", [Msg]),
            flush_messages()
    after 0 -> ok
    end.

start() ->
    io:format("Play MP3 using AtomVM ESP ADF\n"),

    % Create the pipeline
    AudioPipeline = esp_adf_audio_pipeline:init([]),
    MP3File = atomvm:read_priv(?MODULE, "adf_music_mono.mp3"),
    io:format("MP3File: ~B bytes\n", [byte_size(MP3File)]),

    MP3Decoder = esp_adf_mp3_decoder:init([]),
    ok = esp_adf_audio_element:set_read_binary(MP3Decoder, MP3File),
    ok = esp_adf_audio_pipeline:register(AudioPipeline, MP3Decoder, <<"mp3">>),

    Filter = esp_adf_rsp_filter:init([
        {src_rate, 44100}, {src_ch, 1}, {dest_rate, 44100}, {dest_ch, 2}
    ]),
    ok = esp_adf_audio_pipeline:register(AudioPipeline, Filter, <<"filter">>),

    % We know the sample sound is 44.1 kHz, stereo
    I2SOutput = esp_adf_i2s_output:init([
        {rate, 44100},
        {bits, 16},
        {gpio_bclk, ?I2S_BCLK_GPIO},
        {gpio_lrclk, ?I2S_LRC_GPIO},
        {gpio_dout, ?I2S_DIN_GPIO}
    ]),
    ok = esp_adf_audio_pipeline:register(AudioPipeline, I2SOutput, <<"i2s">>),
    ok = esp_adf_audio_pipeline:link(AudioPipeline, [<<"mp3">>, <<"filter">>, <<"i2s">>]),

    % Start playing the sound
    ok = esp_adf_audio_pipeline:run(AudioPipeline),

    % Music info means we can fetch info and data will be meaningful
    ok =
        receive
            {audio_element, MP3Decoder, music_info} -> ok
        after 500 -> timeout
        end,
    MP3Info = esp_adf_audio_element:getinfo(MP3Decoder),

    % We can verify that the provided MP3 is mono
    1 = maps:get(channels, MP3Info),
    44100 = maps:get(sample_rates, MP3Info),

    % However, duration is not meaningful at this point.
    0 = maps:get(duration, MP3Info),

    % We know the sound is about 7 seconds long
    ok =
        receive
            {audio_element, I2SOutput, {status, state_finished}} -> ok
        after 7000 -> timeout
        end,

    % Once we're done, we can stop everything
    ok = esp_adf_audio_pipeline:stop(AudioPipeline),

    % Ensure every element has stopped so wait_for_stop will not block long.
    ok =
        receive
            {audio_element, MP3Decoder, {status, state_stopped}} -> ok
        after 500 -> timeout
        end,
    ok =
        receive
            {audio_element, Filter, {status, state_stopped}} -> ok
        after 500 -> timeout
        end,
    ok =
        receive
            {audio_element, I2SOutput, {status, state_stopped}} -> ok
        after 500 -> timeout
        end,

    % This is the proper cleanup
    ok = esp_adf_audio_pipeline:wait_for_stop(AudioPipeline),
    ok = esp_adf_audio_pipeline:terminate(AudioPipeline),
    ok = esp_adf_audio_pipeline:unregister(AudioPipeline, MP3Decoder),
    ok = esp_adf_audio_pipeline:unregister(AudioPipeline, Filter),
    ok = esp_adf_audio_pipeline:unregister(AudioPipeline, I2SOutput),

    % Additional messages were not processed, ensure they do not stay in the queue
    ok = flush_messages(),
    ok.
