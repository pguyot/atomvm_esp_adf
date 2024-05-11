play_mp3_mono_i2s
=====

Sample code to plan a mono MP3 sound on an I2S card.

The sound is upsampled to stereo before data is sent to I2S.

It was tested with an esp32c3 card connected to a MAX98357A module.

The MAX98357A module is connected to the following esp32c3 gpios:

- LRCLK: gpio 7
- BCLK: gpio 6
- DIN: gpio 5

The MAX98357A module doesn't require any MCLK, so this example doesn't
configure a gpio for it.

Build and flash
---------------

    $ rebar3 atomvm esp32_flash -p /dev/cu.usbmodem*
