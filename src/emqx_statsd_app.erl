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

-module(emqx_statsd_app).

-behaviour(application).

-emqx_plugin(?MODULE).

%% Application callbacks
-export([ start/2
        , stop/1
        ]).

-define(APP, emqx_statsd).

start(_StartType, _StartArgs) ->
    SampleTimeInterval = application:get_env(?APP, sample_time_interval, 10000),
    FlushTimeInterval = application:get_env(?APP, flush_time_interval, 10000),
    Host = application:get_env(?APP, host, {127, 0, 0, 1}),
    Port = application:get_env(?APP, port, 8125),
    Prefix = application:get_env(?APP, prefix, undefined),
    Tags = application:get_env(?APP, tags, []),
    BatchSize = application:get_env(?APP, batch_size, 20),
    ok = estatsd:start_link([{host, Host},
                             {port, Port},
                             {prefix, Prefix},
                             {tags, Tags},
                             {batch_size, BatchSize}]),
    emqx_statsd_sup:start_link([{sample_time_interval, SampleTimeInterval},
                                {flush_time_interval, FlushTimeInterval}]).

stop(_State) ->
    ok.

