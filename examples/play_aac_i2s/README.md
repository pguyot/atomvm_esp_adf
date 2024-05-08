play_aac_i2s
=====

Sample code to plan an AAC sound on an I2S card.

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
