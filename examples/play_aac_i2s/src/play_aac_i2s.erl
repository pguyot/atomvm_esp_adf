%%%-------------------------------------------------------------------
%% @doc Example code to play an AAC file over i2s
%% @end
%%%-------------------------------------------------------------------

-module(play_aac_i2s).

-export([start/0]).

%% GPIO configuration

-define(I2S_LRC_GPIO, 7).
-define(I2S_BCLK_GPIO, 6).
-define(I2S_DIN_GPIO, 5).

start() ->
    io:format("Play AAC using AtomVM ESP ADF\n"),

    AudioPipeline = esp_adf_audio_pipeline:init([]),

    AACFile = atomvm:read_priv(?MODULE, "adf_music.aac"),

    io:format("AACFile: ~B bytes\n", [byte_size(AACFile)]),

    AACDecoder = esp_adf_aac_decoder:init([]),
    ok = esp_adf_audio_element:set_read_binary(AACDecoder, AACFile),
    ok = esp_adf_audio_pipeline:register(AudioPipeline, AACDecoder, <<"aac">>),

    I2SOutput = esp_adf_i2s_output:init([
        {rate, 44100},
        {bits, 16},
        {gpio_bclk, ?I2S_BCLK_GPIO},
        {gpio_lrclk, ?I2S_LRC_GPIO},
        {gpio_dout, ?I2S_DIN_GPIO}
    ]),
    ok = esp_adf_audio_pipeline:register(AudioPipeline, I2SOutput, <<"i2s">>),

    ok = esp_adf_audio_pipeline:link(AudioPipeline, [<<"aac">>, <<"i2s">>]),

    ok = esp_adf_audio_pipeline:run(AudioPipeline),

    timer:sleep(7000),

    ok = esp_adf_audio_pipeline:stop(AudioPipeline),
    ok = esp_adf_audio_pipeline:wait_for_stop(AudioPipeline),
    ok = esp_adf_audio_pipeline:terminate(AudioPipeline),
    ok = esp_adf_audio_pipeline:unregister(AudioPipeline, AACDecoder),
    ok = esp_adf_audio_pipeline:unregister(AudioPipeline, I2SOutput),

    ok.
