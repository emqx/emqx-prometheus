%% Copyright (c) 2013-2019 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(emqx_statsd_app).

-behaviour(application).

-emqx_plugin(?MODULE).

%% Application callbacks
-export([start/2, stop/1]).

-define(APP, emqx_statsd).

start(_StartType, _StartArgs) ->
	PushGateway = application:get_env(?APP, push_gateway, "http://127.0.0.1:9091"),
	Interval = application:get_env(?APP, interval, 5000),
    emqx_statsd_sup:start_link(PushGateway, Interval).

stop(_State) ->
    ok.

