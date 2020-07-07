%%--------------------------------------------------------------------
%% Copyright (c) 2020 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(emqx_statsd_SUITE).

-compile(export_all).
-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

all() -> emqx_ct:all(?MODULE).

init_per_suite(Config) ->
    emqx_ct_helpers:start_apps([emqx_statsd], fun set_special_configs/1),
    Config.

end_per_suite(_Config) ->
    emqx_ct_helpers:stop_apps([emqx_statsd]).

set_special_configs(emqx_statsd) ->
    application:set_env(emqx_statsd, sample_time_interval, 500),
    application:set_env(emqx_statsd, flush_time_interval, 500);
set_special_configs(_App) -> ok.

t_sample(_) ->
    {ok, Socket} = gen_udp:open(8125),
    Packet = should_receive(800),
    Metrics = re:split(Packet, <<"\n">>, [{return, list}]),
    ?assertEqual(length(Metrics), length(emqx_statsd:schema())),
    {Name, _, _} = lists:nth(2, emqx_statsd:schema()),
    {ReceivedName, Value, _} = list_to_tuple(re:split(lists:nth(2, Metrics), "[:\\|]", [{return, list}])),
    ?assertEqual(Name, list_to_atom(ReceivedName)),
    {ok, C} = emqtt:start_link([{clientid, <<"client1">>}]),
    {ok, _} = emqtt:connect(C),
    Packet2 = try_receive_and_return_the_last(2),
    Metrics2 = re:split(Packet2, <<"\n">>, [{return, list}]),
    {ReceivedName, Value2, _} = list_to_tuple(re:split(lists:nth(2, Metrics2), "[:\\|]", [{return, list}])),
    ?assertEqual(list_to_integer(Value) + 1, list_to_integer(Value2)),
    gen_udp:close(Socket).


should_receive(Timeout) ->
    receive
        {udp, _, _, _, Packet} ->
            Packet
    after Timeout ->
        ct:fail(should_recv_packet)
    end.

try_receive_and_return_the_last(0) ->
    should_receive(600);
try_receive_and_return_the_last(Cnt) ->
    should_receive(600),
    try_receive_and_return_the_last(Cnt - 1).