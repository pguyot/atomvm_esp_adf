% SPDX-License-Identifier: MIT
-module(esp_adf_audio_pipeline).

-export_type([audio_pipeline/0]).

-export([
    init/1,
    register/3,
    unregister/2,
    link/2,
    run/1,
    stop/1,
    wait_for_stop/1,
    terminate/1
]).

% Resource for audio pipelines
-opaque audio_pipeline() :: binary().

%% @doc Create a pipeline
%% @param Cfg pipeline configuration
%% @return ok
-spec init([]) -> audio_pipeline().
init(_Cfg) ->
    erlang:nif_error(undefined).

%% @doc Register an audio element. Elements should be registered in the link
%% order as they are sometimes processed in registration order.
%% While an element is registered, it is not garbage collected. It shall be
%% unregistered to avoid any memory leak.
%% @param AudioPipeline pipeline to register the element to
%% @param AudioElement element to register
%% @param Name name of the element in the pipeline
%% @return ok
-spec register(audio_pipeline(), esp_adf_audio_element:audio_element(), atom() | iodata()) -> ok.
register(_AudioPipeline, _AudioElement, _Name) ->
    erlang:nif_error(undefined).

%% @doc Unregister an audio element.
%% @param AudioPipeline pipeline to unregister the element from
%% @param AudioElement element to unregister
%% @return ok
-spec unregister(audio_pipeline(), esp_adf_audio_element:audio_element()) -> ok.
unregister(_AudioPipeline, _AudioElement) ->
    erlang:nif_error(undefined).

%% @doc Link audio elements, i.e. define the pipeline order.
%% @param AudioPipeline pipeline to link the registered elements of
%% @param Names elements to link, in the processing order
%% @return ok
-spec link(audio_pipeline(), [atom() | iodata()]) -> ok.
link(_AudioPipeline, _Names) ->
    erlang:nif_error(undefined).

%% @doc Run the pipeline (asynchronously). This function must be called only
%% once until terminate/1 is called.
%% @param AudioPipeline pipeline to run
%% @return ok
-spec run(audio_pipeline()) -> ok.
run(_AudioPipeline) ->
    erlang:nif_error(undefined).

%% @doc Stop the pipeline. This function initiates stop and returns immediatly.
%% @param AudioPipeline pipeline to stop
%% @return ok
-spec stop(audio_pipeline()) -> ok.
stop(_AudioPipeline) ->
    erlang:nif_error(undefined).

%% @doc Wait for the pipeline to stop. This function is a dirty nif, i.e. it
%% blocks the scheduler until the pipeline is stopped. So users should use
%% events (yet to be ported) to only call it when the pipeline is stopped.
%% @param AudioPipeline pipeline to wait to be stopped
%% @return ok
-spec wait_for_stop(audio_pipeline()) -> ok.
wait_for_stop(_AudioPipeline) ->
    erlang:nif_error(undefined).

%% @doc Terminate the pipeline. This should be called once done with the
%% pipeline. To prevent crashes, the pipeline is held until terminate is
%% called.
%% @param AudioPipeline pipeline to terminate
%% @return ok
-spec terminate(audio_pipeline()) -> ok.
terminate(_AudioPipeline) ->
    erlang:nif_error(undefined).
