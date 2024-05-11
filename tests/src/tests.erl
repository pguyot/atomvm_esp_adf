% SPDX-License-Identifier: MIT
-module(tests).

-include_lib("eunit/include/eunit.hrl").

-export([start/0]).

start() ->
    run_tests().

% On SMP, memory(binary) is not immediatly reduced after last process died.
loop_memory_binary_size(0, _MemoryBinarySize) ->
    timeout;
loop_memory_binary_size(N, MemoryBinarySize) ->
    CurrentMemoryBinarySize = erlang:memory(binary),
    if
        CurrentMemoryBinarySize =:= MemoryBinarySize ->
            ok;
        true ->
            erlang:garbage_collect(),
            timer:sleep(100),
            loop_memory_binary_size(N - 1, MemoryBinarySize)
    end.

mp3_decoder_gc_test() ->
    MemoryBinarySize = erlang:memory(binary),
    {Pid, MonitorRef} = spawn_opt(
        fun() ->
            MP3Decoder = esp_adf_mp3_decoder:init([]),
            MP3Decoder
        end,
        [monitor]
    ),
    receive
        {'DOWN', MonitorRef, process, Pid, normal} -> ok
    end,
    ok = loop_memory_binary_size(5, MemoryBinarySize),
    ok.

mp3_decoder_set_read_binary_gc_test() ->
    MemoryBinarySize = erlang:memory(binary),
    {Pid, MonitorRef} = spawn_opt(
        fun() ->
            MP3File = atomvm:read_priv(?MODULE, "adf_music.mp3"),
            MP3Decoder = esp_adf_mp3_decoder:init([]),
            ok = esp_adf_audio_element:set_read_binary(MP3Decoder, MP3File),
            MP3Decoder
        end,
        [monitor]
    ),
    receive
        {'DOWN', MonitorRef, process, Pid, normal} -> ok
    end,
    ok = loop_memory_binary_size(5, MemoryBinarySize),
    ok.

pipeline_gc_test() ->
    MemoryBinarySize = erlang:memory(binary),
    {Pid, MonitorRef} = spawn_opt(
        fun() ->
            AudioPipeline = esp_adf_audio_pipeline:init([]),
            AudioPipeline
        end,
        [monitor]
    ),
    receive
        {'DOWN', MonitorRef, process, Pid, normal} -> ok
    end,
    ok = loop_memory_binary_size(5, MemoryBinarySize),
    ok.

pipeline_register_unregister_gc_test() ->
    MemoryBinarySize = erlang:memory(binary),
    {Pid, MonitorRef} = spawn_opt(
        fun() ->
            AudioPipeline = esp_adf_audio_pipeline:init([]),
            MP3Decoder = esp_adf_mp3_decoder:init([]),
            ok = esp_adf_audio_pipeline:register(AudioPipeline, MP3Decoder, <<"mp3">>),
            ok = esp_adf_audio_pipeline:unregister(AudioPipeline, MP3Decoder),
            ok
        end,
        [monitor]
    ),
    receive
        {'DOWN', MonitorRef, process, Pid, normal} -> ok
    end,
    ok = loop_memory_binary_size(5, MemoryBinarySize),
    ok.

flush_messages() ->
    receive
        {audio_element, _AudioElement, _Message} = Msg ->
            io:format("~p\n", [Msg]),
            flush_messages()
    after 0 -> ok
    end.

pipeline_link_gc_test() ->
    MemoryBinarySize = erlang:memory(binary),
    {Pid, MonitorRef} = spawn_opt(
        fun() ->
            AudioPipeline = esp_adf_audio_pipeline:init([]),
            AACFile = atomvm:read_priv(?MODULE, "adf_music.aac"),
            AACDecoder = esp_adf_aac_decoder:init([]),
            ok = esp_adf_audio_element:set_read_binary(AACDecoder, AACFile),
            ok = esp_adf_audio_pipeline:register(AudioPipeline, AACDecoder, <<"aac">>),
            ResampleFilter = esp_adf_rsp_filter:init([{src_rate, 44100}, {dest_rate, 48000}]),
            ok = esp_adf_audio_pipeline:register(AudioPipeline, ResampleFilter, <<"filter">>),
            I2SOutput = esp_adf_i2s_output:init([
                {rate, 48000},
                {bits, 16},
                {gpio_bclk, 9},
                {gpio_lrclk, 8},
                {gpio_dout, 7}
            ]),
            ok = esp_adf_audio_pipeline:register(AudioPipeline, I2SOutput, <<"i2s">>),
            ok = esp_adf_audio_pipeline:link(AudioPipeline, [<<"aac">>, <<"filter">>, <<"i2s">>]),
            ok = esp_adf_audio_pipeline:unregister(AudioPipeline, AACDecoder),
            ok = esp_adf_audio_pipeline:unregister(AudioPipeline, ResampleFilter),
            ok = esp_adf_audio_pipeline:unregister(AudioPipeline, I2SOutput),
            ok
        end,
        [monitor]
    ),
    receive
        {'DOWN', MonitorRef, process, Pid, normal} -> ok
    end,
    ok = loop_memory_binary_size(5, MemoryBinarySize),
    ok.

pipeline_run_stop_gc_test() ->
    MemoryBinarySize = erlang:memory(binary),
    {Pid, MonitorRef} = spawn_opt(
        fun() ->
            AudioPipeline = esp_adf_audio_pipeline:init([]),
            MP3File = atomvm:read_priv(?MODULE, "adf_music.mp3"),
            MP3Decoder = esp_adf_mp3_decoder:init([]),
            ok = esp_adf_audio_element:set_read_binary(MP3Decoder, MP3File),
            ok = esp_adf_audio_pipeline:register(AudioPipeline, MP3Decoder, <<"mp3">>),
            ok = esp_adf_audio_pipeline:link(AudioPipeline, [<<"mp3">>]),
            ok = esp_adf_audio_pipeline:run(AudioPipeline),
            ok =
                receive
                    {audio_element, MP3Decoder, {status, state_finished}} -> ok
                after 7000 -> timeout
                end,
            ok = flush_messages(),
            ok = esp_adf_audio_pipeline:stop(AudioPipeline),
            ok =
                receive
                    {audio_element, MP3Decoder, {status, state_stopped}} -> ok
                after 500 -> timeout
                end,
            ok = esp_adf_audio_pipeline:wait_for_stop(AudioPipeline),
            ok = esp_adf_audio_pipeline:terminate(AudioPipeline),
            ok = esp_adf_audio_pipeline:unregister(AudioPipeline, MP3Decoder),
            ok = flush_messages(),
            ok
        end,
        [monitor]
    ),
    receive
        {'DOWN', MonitorRef, process, Pid, normal} -> ok
    end,
    ok = loop_memory_binary_size(5, MemoryBinarySize),
    ok.

mp3_decoder_active_events_test() ->
    AudioPipeline = esp_adf_audio_pipeline:init([]),
    MP3File = atomvm:read_priv(?MODULE, "adf_music.mp3"),
    MP3Decoder = esp_adf_mp3_decoder:init([]),
    ok = esp_adf_audio_element:set_read_binary(MP3Decoder, MP3File),
    ok = esp_adf_audio_pipeline:register(AudioPipeline, MP3Decoder, <<"mp3">>),
    ok = esp_adf_audio_pipeline:link(AudioPipeline, [<<"mp3">>]),
    ok = esp_adf_audio_pipeline:run(AudioPipeline),
    ok =
        receive
            {audio_element, MP3Decoder, {status, state_running}} -> ok
        after 500 -> timeout
        end,
    ok =
        receive
            {audio_element, MP3Decoder, music_info} -> ok
        after 500 -> timeout
        end,
    {ok, SoundDuration} =
        receive
            {audio_element, MP3Decoder, {position, Position}} ->
                mp3 = maps:get(codec_fmt, Position),
                Duration = maps:get(duration, Position),
                {ok, Duration}
        after 500 -> timeout
        end,
    ok =
        receive
            {audio_element, MP3Decoder, {status, state_finished}} -> ok
        after SoundDuration -> timeout
        end,
    ok = esp_adf_audio_pipeline:stop(AudioPipeline),
    ok =
        receive
            {audio_element, MP3Decoder, {status, state_stopped}} -> ok
        after 500 -> timeout
        end,
    ok = esp_adf_audio_pipeline:wait_for_stop(AudioPipeline),
    ok = esp_adf_audio_pipeline:terminate(AudioPipeline),
    ok = esp_adf_audio_pipeline:unregister(AudioPipeline, MP3Decoder),
    ok = flush_messages(),
    ok.

mp3_decoder_passive_events_test() ->
    AudioPipeline = esp_adf_audio_pipeline:init([]),
    MP3File = atomvm:read_priv(?MODULE, "adf_music.mp3"),
    MP3Decoder = esp_adf_mp3_decoder:init([{active, false}]),
    ok = esp_adf_audio_element:set_read_binary(MP3Decoder, MP3File),
    ok = esp_adf_audio_pipeline:register(AudioPipeline, MP3Decoder, <<"mp3">>),
    ok = esp_adf_audio_pipeline:link(AudioPipeline, [<<"mp3">>]),
    ok = esp_adf_audio_pipeline:run(AudioPipeline),
    {ok, {status, state_running}} = esp_adf_audio_element:get_event(MP3Decoder),
    {ok, music_info} = esp_adf_audio_element:get_event(MP3Decoder),
    {ok, {position, Position}} = esp_adf_audio_element:get_event(MP3Decoder),
    mp3 = maps:get(codec_fmt, Position),
    _SoundDuration = maps:get(duration, Position),
    {ok, {status, state_finished}} = esp_adf_audio_element:get_event(MP3Decoder, infinity),
    {error, timeout} = esp_adf_audio_element:get_event(MP3Decoder, 0),
    ok = esp_adf_audio_pipeline:stop(AudioPipeline),
    ok = esp_adf_audio_pipeline:wait_for_stop(AudioPipeline),
    ok = esp_adf_audio_pipeline:terminate(AudioPipeline),
    ok = esp_adf_audio_pipeline:unregister(AudioPipeline, MP3Decoder),
    ok.

get_event_errors_test() ->
    MP3Decoder = esp_adf_mp3_decoder:init([]),
    {error, einval} = esp_adf_audio_element:get_event(MP3Decoder, 0),
    esp_adf_audio_element:setopts(MP3Decoder, [{active, false}]),
    {error, timeout} = esp_adf_audio_element:get_event(MP3Decoder, 0),
    esp_adf_audio_element:setopts(MP3Decoder, [{active, true}]),
    {error, einval} = esp_adf_audio_element:get_event(MP3Decoder, 0),
    ok.

mp3_decoder_active_events_controlling_process_test() ->
    AudioPipeline = esp_adf_audio_pipeline:init([]),
    MP3File = atomvm:read_priv(?MODULE, "adf_music.mp3"),
    MP3Decoder = esp_adf_mp3_decoder:init([]),
    ok = esp_adf_audio_element:set_read_binary(MP3Decoder, MP3File),
    ok = esp_adf_audio_pipeline:register(AudioPipeline, MP3Decoder, <<"mp3">>),
    ok = esp_adf_audio_pipeline:link(AudioPipeline, [<<"mp3">>]),
    {Pid, MonitorRef} = spawn_opt(
        fun() ->
            ok =
                receive
                    {audio_element, MP3Decoder, {status, state_running}} -> ok
                after 500 -> {timeout, status}
                end,
            ok =
                receive
                    {audio_element, MP3Decoder, music_info} -> ok
                after 500 -> timeout
                end,
            MP3Info = esp_adf_audio_element:getinfo(MP3Decoder),
            mp3 = maps:get(codec_fmt, MP3Info),
            {ok, SoundDuration} =
                receive
                    {audio_element, MP3Decoder, {position, Position}} ->
                        mp3 = maps:get(codec_fmt, Position),
                        Duration = maps:get(duration, Position),
                        {ok, Duration}
                after 500 -> timeout
                end,
            ok =
                receive
                    {audio_element, MP3Decoder, {status, state_finished}} -> ok
                after SoundDuration -> timeout
                end,
            ok = flush_messages(),
            ok
        end,
        [monitor]
    ),
    {DeadPid, DeadMonitorRef} = spawn_opt(
        fun() ->
            ok
        end,
        [monitor]
    ),
    normal =
        receive
            {'DOWN', DeadMonitorRef, process, DeadPid, DeadReason} -> DeadReason
        end,
    {error, badarg} = esp_adf_audio_element:controlling_process(MP3Decoder, DeadPid),
    ok = esp_adf_audio_element:controlling_process(MP3Decoder, Pid),
    {error, not_owner} = esp_adf_audio_element:controlling_process(MP3Decoder, Pid),
    ok = esp_adf_audio_pipeline:run(AudioPipeline),
    normal =
        receive
            {'DOWN', MonitorRef, process, Pid, Reason} -> Reason
        end,
    ok = esp_adf_audio_pipeline:stop(AudioPipeline),
    ok = esp_adf_audio_pipeline:wait_for_stop(AudioPipeline),
    ok = esp_adf_audio_pipeline:terminate(AudioPipeline),
    ok = esp_adf_audio_pipeline:unregister(AudioPipeline, MP3Decoder),
    ok = flush_messages(),
    ok.

-define(TIMEOUT, 30000).

run_tests() ->
    Exports = ?MODULE:module_info(exports),
    Tests = collect_tests(Exports, []),
    {Total, Failures} = lists:foldl(
        fun(Test, {AccTotal, AccFailures}) ->
            {Line, TestName, TestFun} = Test,
            case run_test(Line, TestName, TestFun, ?TIMEOUT) of
                ok -> {AccTotal + 1, AccFailures};
                {error, _} -> {AccTotal + 1, AccFailures + 1}
            end
        end,
        {0, 0},
        Tests
    ),
    Outcome =
        case Failures of
            0 -> "OK";
            _ -> "FAIL"
        end,
    io:format("- ~B Tests ~B Failures ~B Ignored ~s\n", [Total, Failures, 0, Outcome]),
    ok.

collect_tests([], Acc) ->
    Acc;
collect_tests([{Func, 0} | Tail], Acc) ->
    case lists:reverse(atom_to_list(Func)) of
        "_tset_" ++ _ ->
            GeneratorResult = ?MODULE:Func(),
            NewAcc = collect_tests_from_generator(Func, GeneratorResult, Acc),
            collect_tests(Tail, NewAcc);
        "tset_" ++ _ ->
            collect_tests(Tail, [{0, Func, fun() -> ?MODULE:Func() end} | Acc]);
        _ ->
            collect_tests(Tail, Acc)
    end;
collect_tests([_ | Tail], Acc) ->
    collect_tests(Tail, Acc).

collect_tests_from_generator(TestCase, {Line, Func}, Acc) when is_function(Func, 0) ->
    lists:reverse([{Line, TestCase, Func} | Acc]);
collect_tests_from_generator(TestCase, List, Acc) when is_list(List) ->
    % List elements must be processed in order
    TestList = collect_tests_from_list(TestCase, List, []),
    lists:reverse(Acc, TestList).

collect_tests_from_list(_TestCase, [], Acc) ->
    lists:flatten(lists:reverse(Acc));
collect_tests_from_list(TestCase, [Head | Tail], Acc) ->
    TestItem = collect_tests_from_generator(TestCase, Head, []),
    collect_tests_from_list(TestCase, Tail, [TestItem | Acc]).

run_test(Line, TestName, TestFun, Timeout) ->
    Self = self(),
    {Pid, MonitorRef} = spawn_opt(
        fun() ->
            io:format("~s:~B:~s...\n", [?MODULE, Line, TestName]),
            try
                Result = TestFun(),
                io:format("~s:~B:~s:PASS:~p\n", [?MODULE, Line, TestName, Result]),
                Self ! {self(), ok}
            catch
                error:Error:StackTrace ->
                    io:format("~s:~B:~s:FAIL:~p\n~p\n", [?MODULE, Line, TestName, Error, StackTrace]),
                    Self ! {self(), {error, failure}}
            end
        end,
        [monitor]
    ),
    receive
        {Pid, Result} ->
            demonitor(MonitorRef, [flush]),
            Result;
        {'DOWN', MonitorRef, process, Pid, Reason} ->
            io:format("~s:~B:~s:FAIL:exited with ~p\n", [?MODULE, Line, TestName, Reason]),
            {error, failure}
    after Timeout ->
        io:format("~s:~B:~s:FAIL:timeout\n", [?MODULE, Line, TestName]),
        {error, timeout}
    end.
