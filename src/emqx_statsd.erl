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

-module(emqx_statsd).

-behaviour(gen_server).

%% Interface
-export([start_link/1]).

%% Internal Exports
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , code_change/3
        , terminate/2
        ]).

-record(state, {
          timer,
          sample_time_interval :: pos_integer(),
          flush_time_interval :: pos_integer(),
          cache :: map()
        }).

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Opts], []).

init([Opts]) ->
    SampleTimeInterval = proplists:get_value(sample_time_interval, Opts),
    Ref = erlang:start_timer(SampleTimeInterval, self(), sample_timeout),
    {ok, #state{timer = Ref, sample_time_interval = SampleTimeInterval}}.

handle_call(_Req, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, Ref, sample_timeout}, State = #state{sample_time_interval = SampleTimeInterval,
                                                           flush_time_interval = FlushTimeInterval,
                                                           timer = Ref,
                                                           cache = Cache}) ->
    RawMetrics = emqx_metrics:all() ++ emqx_stats:getstats(),
    SampleRate = SampleTimeInterval / FlushTimeInterval,
    {StatsdMetrics, NCache} =
        lists:foldl(fun({StatsdMetricName, Type, RawMetricName}, {StatsdMetricsAcc, CacheAcc}) ->
                        Value = proplists:get_value(RawMetricName, RawMetrics),
                        NValue = case Type of
                                    counter ->
                                        Value - maps:get(StatsdMetricName, CacheAcc, 0);
                                    _ -> Value
                                 end,
                        NCacheAcc = maps:put(StatsdMetricName, Value, CacheAcc),
                        {[{Type, StatsdMetricName, NValue, SampleRate, []} | StatsdMetricsAcc], NCacheAcc}
                    end, {[], Cache}, schema()),
    estatsd:submit(StatsdMetrics),
    {noreply, State#state{cache = NCache,
                          timer = erlang:start_timer(SampleTimeInterval, self(), sample_timeout)}};

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

schema() ->
    [ {'clients',                                      counter, 'connections.count'}
    , {'clients.maximum.historical',                   gauge,   'connections.max'}
    , {'sessions',                                     counter, 'sessions.count'}
    , {'sessions.maximum.historical',                  gauge,   'sessions.max'}
    , {'topics',                                       counter, 'topics.count'}
    , {'topics.maximum.historical',                    gauge,   'topics.max'}
    , {'subscriptions',                                counter, 'subscriptions.count'}
    , {'subscriptions.maximum.historical',             gauge,   'subscriptions.max'}
    , {'shared_subscriptions',                         counter, 'subscriptions.shared.count'}
    , {'shared_subscriptions.maximum.historical',      gauge,   'subscriptions.shared.max'}
    , {'bytes.received',                               counter, 'bytes.received'}
    , {'bytes.sent',                                   counter, 'bytes.sent'}
    , {'packets.received',                             counter, 'packets.received'}
    , {'packets.sent',                                 counter, 'packets.sent'}
    , {'packets.connect.received',                     counter, 'packets.connect.received'}
    , {'packets.connack.sent',                         counter, 'packets.connack.sent'}
    , {'packets.publish.received',                     counter, 'packets.publish.received'}
    , {'packets.publish.sent',                         counter, 'packets.publish.sent'}
    , {'packets.publish.qos0.received',                counter, 'messages.qos0.received'}
    , {'packets.publish.qos0.sent',                    counter, 'messages.qos0.sent'}
    , {'packets.publish.qos1.received',                counter, 'messages.qos1.received'}
    , {'packets.publish.qos1.sent',                    counter, 'messages.qos1.sent'}
    , {'packets.publish.qos2.received',                counter, 'messages.qos2.received'}
    , {'packets.publish.qos2.sent',                    counter, 'messages.qos2.sent'}
    , {'packets.publish.retained',                     counter, 'retained.count'}
    , {'packets.publish.retained.maximum.historical',  gauge,   'retained.max'}
    , {'packets.publish.delayed',                      counter, 'messages.delayed'}
    , {'packets.publish.forwarded_to_other_node',      counter, 'messages.forward'}
    , {'packets.puback.received',                      counter, 'packets.puback.received'}
    , {'packets.puback.sent',                          counter, 'packets.puback.sent'}
    , {'packets.pubrec.received',                      counter, 'packets.pubrec.received'}
    , {'packets.pubrec.sent',                          counter, 'packets.pubrec.sent'}
    , {'packets.pubrel.received',                      counter, 'packets.pubrel.received'}
    , {'packets.pubrel.sent',                          counter, 'packets.pubrel.sent'}
    , {'packets.pubcomp.received',                     counter, 'packets.pubcomp.received'}
    , {'packets.pubcomp.sent',                         counter, 'packets.pubcomp.sent'}
    , {'packets.subscribe.received',                   counter, 'packets.subscribe.received'}
    , {'packets.suback.sent',                          counter, 'packets.subscribe.sent'}
    , {'packets.unsubscribe.received',                 counter, 'packets.unsubscribe.received'}
    , {'packets.unsuback.sent',                        counter, 'packets.unsubscribe.sent'}
    , {'packets.pingreq.received',                     counter, 'packets.pingreq.received'}
    , {'packets.pingresp.sent',                        counter, 'packets.pingresp.sent'}
    , {'packets.disconnect.received',                  counter, 'packets.disconnect.received'}
    , {'packets.disconnect.sent',                      counter, 'packets.disconnect.sent'}
    , {'packets.auth.recevied',                        counter, 'packets.auth.recevied'}
    , {'packets.auth.sent',                            counter, 'packets.auth.sent'}
    , {'events.client.connected',                      gauge,   'client.connected'}
    , {'events.client.disconnected',                   gauge,   'client.disconnected'}
    , {'events.client.authenticate',                   gauge,   'client.authenticate'}
    , {'events.client.pass_authenticate_by_anonymous', gauge,   'client.auth.anonymous'}
    , {'events.client.subscribe',                      gauge,   'client.subscribe'}
    , {'events.client.unsubscribe',                    gauge,   'client.unsubscribe'}
    , {'events.client.check_access_control_list',      gauge,   'client.check_acl'}
    , {'events.session.created',                       gauge,   'session.created'}
    , {'events.session.resumed',                       gauge,   'session.resumed'}
    , {'events.session.take_overed',                   gauge,   'session.takeovered'}
    , {'events.session.discarded',                     gauge,   'session.discarded'}
    , {'events.session.terminated',                    gauge,   'session.terminated'}
    ].
