%%%-------------------------------------------------------------------
%% @doc Example code to play an MP3 file over i2s
%% @end
%%%-------------------------------------------------------------------

-module(play_mp3_i2s).

-export([start/0]).

%% GPIO configuration

-define(I2S_LRC_GPIO, 7).
-define(I2S_BCLK_GPIO, 6).
-define(I2S_DIN_GPIO, 5).

start() ->
    io:format("Play MP3 using AtomVM ESP ADF\n"),

    AudioPipeline = esp_adf_audio_pipeline:init([]),

    MP3File = atomvm:read_priv(?MODULE, "adf_music.mp3"),

    io:format("MP3File: ~B bytes\n", [byte_size(MP3File)]),

    MP3Decoder = esp_adf_mp3_decoder:init([]),
    ok = esp_adf_audio_element:set_read_binary(MP3Decoder, MP3File),
    ok = esp_adf_audio_pipeline:register(AudioPipeline, MP3Decoder, <<"mp3">>),

    % We know the sample sound is 44.1 kHz, stereo
    I2SOutput = esp_adf_i2s_output:init([
        {rate, 44100},
        {bits, 16},
        {gpio_bclk, ?I2S_BCLK_GPIO},
        {gpio_lrclk, ?I2S_LRC_GPIO},
        {gpio_dout, ?I2S_DIN_GPIO}
    ]),
    ok = esp_adf_audio_pipeline:register(AudioPipeline, I2SOutput, <<"i2s">>),

    ok = esp_adf_audio_pipeline:link(AudioPipeline, [<<"mp3">>, <<"i2s">>]),

    ok = esp_adf_audio_pipeline:run(AudioPipeline),

    % We know the sample sound is about 7 seconds long.
    ok =
        receive
            {audio_element, I2SOutput, {status, state_finished}} -> ok
        after 7000 -> timeout
        end,

    ok = esp_adf_audio_pipeline:stop(AudioPipeline),
    ok = esp_adf_audio_pipeline:wait_for_stop(AudioPipeline),
    ok = esp_adf_audio_pipeline:terminate(AudioPipeline),
    ok = esp_adf_audio_pipeline:unregister(AudioPipeline, MP3Decoder),
    ok = esp_adf_audio_pipeline:unregister(AudioPipeline, I2SOutput),

    ok.
