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

-module(emqx_statsd_sup).

-behaviour(supervisor).

-export([start_link/2]).

-export([init/1]).

start_link(PushGateway, Interval) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [PushGateway, Interval]).

init([PushGateway, Interval]) ->
    {ok, {#{strategy => one_for_one, intensity => 10, period => 100},
          [#{id       => emqx_statsd,
             start    => {emqx_statsd, start_link, [PushGateway, Interval]},
             restart  => permanent,
             shutdown => 5000,
             type     => worker,
             modules  => [emqx_statsd]}]}}.

