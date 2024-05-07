% SPDX-License-Identifier: MIT
-module(esp_adf_i2s_output).

-export([init/1]).

-type i2s_output_option() ::
    {rate, pos_integer()}
    | {gpio_mclk, -1 | non_neg_integer()}
    | {gpio_bclk, non_neg_integer()}
    | {gpio_lrclk, non_neg_integer()}
    | {gpio_dout, non_neg_integer()}.

%% @doc Create a handle to an i2s output audio element.
%% `gpio_bclk', `gpio_lrclk' and `gpio_dout' options are required.
%% @param Cfg configuration
-spec init([i2s_output_option()]) -> audio_element:audio_element().
init(_Cfg) ->
    erlang:nif_error(undefined).
