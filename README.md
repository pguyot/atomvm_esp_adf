# AtomVM driver for Espressif Audio Framework

## Usage:

1. Clone this repository into atomvm/src/platforms/esp32/components with submodules

```bash
cd AtomVM/src/platforms/esp32/components/
git clone --recurse-submodules https://github.com/pguyot/atomvm_esp_adf.git
```

2. Build and flash AtomVM

Use a command sequence such as: (port name depends on OS)

```bash
cd AtomVM/src/platforms/esp32/
idf.py build
idf.py -p /dev/cu.usbmodem14401 flash
```

3. Use `atomvm_esp_adf_*` modules in your code.

```erlang
AudioPipeline = esp_adf_audio_pipeline:init([]),

MP3File = atomvm:read_priv(?MODULE, "adf_music.mp3"),
MP3Decoder = esp_adf_mp3_decoder:init([]),
ok = esp_adf_audio_element:set_read_binary(MP3Decoder, MP3File),
ok = esp_adf_audio_pipeline:register(AudioPipeline, MP3Decoder, <<"mp3">>),

I2SOutput = esp_adf_i2s_output:init([{gpio_bclk, ?MAX_BCLK_GPIO}, {gpio_lrclk, ?MAX_LRC_GPIO}, {gpio_dout, ?MAX_DIN_GPIO}]),
ok = esp_adf_audio_pipeline:register(AudioPipeline, I2SOutput, <<"i2s">>),

ok = esp_adf_audio_pipeline:link(AudioPipeline, [<<"mp3">>, <<"i2s">>]),

ok = esp_adf_audio_pipeline:run(AudioPipeline),

...

ok = esp_adf_audio_pipeline:stop(AudioPipeline),
ok = esp_adf_audio_pipeline:wait_for_stop(AudioPipeline),
ok = esp_adf_audio_pipeline:terminate(AudioPipeline),
ok = esp_adf_audio_pipeline:unregister(AudioPipeline, MP3Decoder),
ok = esp_adf_audio_pipeline:unregister(AudioPipeline, I2SOutput),
```
