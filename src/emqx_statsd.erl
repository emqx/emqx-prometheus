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

-module(emqx_statsd).

-include_lib("prometheus/include/prometheus.hrl").

-behaviour(gen_server).

-behaviour(prometheus_collector).

-import(prometheus_model_helpers, [create_mf/5,
                                   gauge_metrics/1,
                                   gauge_metric/1,
                                   gauge_metric/2,
                                   counter_metric/1,
                                   counter_metric/2,
                                   counter_metrics/1]).

%% Interface
-export([start_link/2]).

%% Internal Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
         terminate/2]).

%% prometheus_collector callback
-export([deregister_cleanup/1, collect_mf/2, collect_metrics/2]).

-define(C(K, L), proplists:get_value(K, L, 0)).

-define(TIMER_MSG, '#interval').

-record(state, {push_gateway, timer, interval}).

start_link(PushGateway, Interval) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [PushGateway, Interval], []).

init([PushGateway, Interval]) ->
    Ref = erlang:start_timer(Interval, self(), ?TIMER_MSG),
    {ok, #state{timer = Ref, push_gateway = PushGateway, interval = Interval}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, R, ?TIMER_MSG}, S = #state{interval=I, timer=R, push_gateway=Uri}) ->
    [Name, Ip] = string:tokens(atom_to_list(node()), "@"),
    Url = lists:concat([Uri, "/metrics/job/", Name, "/instance/",Name, "~", Ip]),
    Data = prometheus_text_format:format(),
    httpc:request(post, {Url, [], "text/plain", Data}, [{autoredirect, true}], []),
    {noreply, S#state{timer = erlang:start_timer(I, self(), ?TIMER_MSG)}};

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

deregister_cleanup(_Registry) ->
    ok.

collect_mf(_Registry, Callback) ->
    Metrics = emqx_metrics:all(),
    Stats = emqx_stats:getstats(),
    VMData = emqx_vm_data(),
    [add_collect_family(Name, Metrics, Callback, counter) || Name <- emqx_metrics()],
    [add_collect_family(Name, Stats, Callback, gauge) || Name <- emqx_stats()],
    [add_collect_family(Name, VMData, Callback, gauge) || Name <- emqx_vm()],
    [add_collect_family(Name, Metrics, Callback, counter) || Name <- emqx_messages()],
    ok.

collect_metrics(Name, Metrics) ->
    emqx_collect(Name, Metrics).

add_collect_family(Name, Data, Callback, Type) ->
    Callback(create_schema(Name, "",  Data, Type)).

create_schema(Name, Help, Data, Type) ->
  create_mf(Name, Help, Type, ?MODULE, Data).

%% auth
emqx_collect(emqx_packets_auth, Metrics) ->
    counter_metric(?C('packets/auth', Metrics));

%% received/sent
emqx_collect(emqx_packets_received, Metrics) ->
    counter_metric(?C('packets/received', Metrics));
emqx_collect(emqx_packets_sent, Metrics) ->
    counter_metric(?C('packets/sent', Metrics));

%% connect/disconnect
emqx_collect(emqx_packets_connect, Metrics) ->
    counter_metric(?C('packets/connect', Metrics));
emqx_collect(emqx_packets_connack, Metrics) ->
    counter_metric(?C('packets/connack', Metrics));
emqx_collect(emqx_packets_disconnect_received, Metrics) ->
    counter_metric(?C('packets/disconnect/received', Metrics));
emqx_collect(emqx_packets_disconnect_sent, Metrics) ->
    counter_metric(?C('packets/disconnect/sent', Metrics));

%% sub/unsub
emqx_collect(emqx_packets_subscribe, Metrics) ->
    counter_metric(?C('packets/subscribe', Metrics));
emqx_collect(emqx_packets_suback, Metrics) ->
    counter_metric(?C('packets/suback', Metrics));
emqx_collect(emqx_packets_unsubscribe, Metrics) ->
    counter_metric(?C('packets/unsubscribe', Metrics));
emqx_collect(emqx_packets_unsuback, Metrics) ->
    counter_metric(?C('packets/unsuback', Metrics));

%% publish/puback
emqx_collect(emqx_packets_publish_received, Metrics) ->
    counter_metric(?C('packets/publish/received', Metrics));
emqx_collect(emqx_packets_publish_sent, Metrics) ->
    counter_metric(?C('packets/publish/sent', Metrics));
emqx_collect(emqx_packets_puback_received, Metrics) ->
    counter_metric(?C('packets/puback/received', Metrics));
emqx_collect(emqx_packets_puback_sent, Metrics) ->
    counter_metric(?C('packets/puback/sent', Metrics));
emqx_collect(emqx_packets_puback_missed, Metrics) ->
    counter_metric(?C('packets/puback/missed', Metrics));
emqx_collect(emqx_packets_pubrec_received, Metrics) ->
    counter_metric(?C('packets/pubrec/received', Metrics));
emqx_collect(emqx_packets_pubrec_sent, Metrics) ->
    counter_metric(?C('packets/pubrec/sent', Metrics));
emqx_collect(emqx_packets_pubrec_missed, Metrics) ->
    counter_metric(?C('packets/pubrec/missed', Metrics));
emqx_collect(emqx_packets_pubrel_received, Metrics) ->
    counter_metric(?C('packets/pubrel/received', Metrics));
emqx_collect(emqx_packets_pubrel_sent, Metrics) ->
    counter_metric(?C('packets/pubrel/sent', Metrics));
emqx_collect(emqx_packets_pubrel_missed, Metrics) ->
    counter_metric(?C('packets/pubrel/missed', Metrics));
emqx_collect(emqx_packets_pubcomp_received, Metrics) ->
    counter_metric(?C('packets/pubcomp/received', Metrics));
emqx_collect(emqx_packets_pubcomp_sent, Metrics) ->
    counter_metric(?C('packets/pubcomp/sent', Metrics));
emqx_collect(emqx_packets_pubcomp_missed, Metrics) ->
    counter_metric(?C('packets/pubcomp/missed', Metrics));

%% pingreq
emqx_collect(emqx_packets_pingreq, Metrics) ->
    counter_metric(?C('packets/pingreq', Metrics));
emqx_collect(emqx_packets_pingresp, Metrics) ->
    counter_metric(?C('packets/pingresp', Metrics));

%% bytes
emqx_collect(emqx_bytes_received, Metrics) ->
    counter_metric(?C('bytes/received', Metrics));
emqx_collect(emqx_bytes_sent, Metrics) ->
    counter_metric(?C('bytes/sent', Metrics));

%% connections
emqx_collect(emqx_connections_count, Stats) ->
    gauge_metric(?C('connections/count', Stats));
emqx_collect(emqx_connections_max, Stats) ->
    gauge_metric(?C('connections/max', Stats));

%% retained
emqx_collect(emqx_retained_count, Stats) ->
    gauge_metric(?C('retained/count', Stats));
emqx_collect(emqx_retained_max, Stats) ->
    gauge_metric(?C('retained/max', Stats));

%% routes
emqx_collect(emqx_routes_count, Stats) ->
    gauge_metric(?C('routes/count', Stats));
emqx_collect(emqx_routes_max, Stats) ->
    gauge_metric(?C('routes/max', Stats));

%% sessions
emqx_collect(emqx_sessions_count, Stats) ->
    gauge_metric(?C('sessions/count', Stats));
emqx_collect(emqx_sessions_max, Stats) ->
    gauge_metric(?C('sessions/max', Stats));

%% subscriptions
emqx_collect(emqx_subscriptions_count, Stats) ->
    gauge_metric(?C('subscriptions/count', Stats));
emqx_collect(emqx_subscriptions_max, Stats) ->
    gauge_metric(?C('subscriptions/max', Stats));

%% topics
emqx_collect(emqx_topics_count, Stats) ->
    gauge_metric(?C('topics/count', Stats));
emqx_collect(emqx_topics_max, Stats) ->
    gauge_metric(?C('topics/max', Stats));

emqx_collect(emqx_vm_cpu_use, VMData) ->
    gauge_metric(?C(cpu_use, VMData));

emqx_collect(emqx_vm_cpu_idle, VMData) ->
    gauge_metric(?C(cpu_idle, VMData));

emqx_collect(emqx_vm_run_queue, VMData) ->
    gauge_metric(?C(run_queue, VMData));

emqx_collect(emqx_vm_process_messages_in_queues, VMData) ->
    gauge_metric(?C(process_total_messages, VMData));

%% messages
emqx_collect(emqx_messages_received, Metrics) ->
    counter_metric(?C('messages/received', Metrics));

emqx_collect(emqx_messages_sent, Metrics) ->
    counter_metric(?C('messages/sent', Metrics));

emqx_collect(emqx_messages_dropped, Metrics) ->
    counter_metric(?C('messages/dropped', Metrics));

emqx_collect(emqx_messages_retained, Metrics) ->
    counter_metric(?C('messages/retained', Metrics));

emqx_collect(emqx_messages_qos0_received, Metrics) ->
    counter_metric(?C('messages/qos0/received', Metrics));
emqx_collect(emqx_messages_qos0_sent, Metrics) ->
    counter_metric(?C('messages/qos0/sent', Metrics));

emqx_collect(emqx_messages_qos1_received, Metrics) ->
    counter_metric(?C('messages/qos1/received', Metrics));
emqx_collect(emqx_messages_qos1_sent, Metrics) ->
    counter_metric(?C('messages/qos1/sent', Metrics));

emqx_collect(emqx_messages_qos2_received, Metrics) ->
    counter_metric(?C('messages/qos2/received', Metrics));
emqx_collect(emqx_messages_qos2_expired, Metrics) ->
    counter_metric(?C('messages/qos2/expired', Metrics));
emqx_collect(emqx_messages_qos2_sent, Metrics) ->
    counter_metric(?C('messages/qos2/sent', Metrics));
emqx_collect(emqx_messages_qos2_dropped, Metrics) ->
    counter_metric(?C('messages/qos2/dropped', Metrics));
emqx_collect(emqx_messages_qos2_forward, Metrics) ->
    counter_metric(?C('messages/qos2/forward', Metrics));
emqx_collect(emqx_message_forward, Metrics) ->
    counter_metric(?C('messages/forward', Metrics));
emqx_collect(emqx_message_expired, Metrics) ->
    counter_metric(?C('messages/expired', Metrics)).

emqx_metrics() ->
    [emqx_packets_auth,
     emqx_packets_received,
     emqx_packets_connect,
     emqx_packets_connack,
     emqx_packets_disconnect_received,
     emqx_packets_disconnect_sent,
     emqx_packets_subscribe,
     emqx_packets_suback,
     emqx_packets_unsubscribe,
     emqx_packets_unsuback,
     emqx_packets_publish_received,
     emqx_packets_publish_sent,
     emqx_packets_puback_received,
     emqx_packets_puback_sent,
     emqx_packets_puback_missed,
     emqx_packets_pubrec_received,
     emqx_packets_pubrec_sent,
     emqx_packets_pubrec_missed,
     emqx_packets_pubrel_received,
     emqx_packets_pubrel_sent,
     emqx_packets_pubrel_missed,
     emqx_packets_pubcomp_received,
     emqx_packets_pubcomp_sent,
     emqx_packets_pubcomp_missed,
     emqx_packets_pingreq,
     emqx_packets_pingresp,
     emqx_bytes_received,
     emqx_bytes_sent].

emqx_stats() ->
    [emqx_connections_count,
     emqx_connections_max,
     emqx_retained_count,
     emqx_retained_max,
     emqx_routes_count,
     emqx_routes_max,
     emqx_sessions_count,
     emqx_sessions_max,
     emqx_subscriptions_count,
     emqx_subscriptions_max,
     emqx_topics_count,
     emqx_topics_max].

emqx_messages() ->
    [emqx_messages_received,
     emqx_messages_sent,
     emqx_messages_dropped,
     emqx_messages_retained,
     emqx_messages_qos0_received,
     emqx_messages_qos0_sent,
     emqx_messages_qos1_received,
     emqx_messages_qos1_sent,
     emqx_messages_qos2_received,
     emqx_messages_qos2_expired,
     emqx_messages_qos2_sent,
     emqx_messages_qos2_dropped,
     emqx_messages_qos2_forward
    ].

emqx_vm() ->
    [emqx_vm_cpu_use,
     emqx_vm_cpu_idle,
     emqx_vm_run_queue,
     emqx_vm_process_messages_in_queues].

emqx_vm_data() ->
    {_Num, _Use, IdleList, _} = cpu_sup:util([detailed]),
    Idle = ?C(idle, IdleList),
    RunQueue = erlang:statistics(run_queue),

    ProcessTotalMessages = lists:foldl(
    fun(Pid, Acc) ->
        case process_info(Pid, message_queue_len) of
            undefined -> Acc;
            {message_queue_len, Count} -> Count+Acc
        end
    end, 0, processes()),
    [{run_queue, RunQueue},
     {process_total_messages, ProcessTotalMessages},
     {cpu_idle, Idle},
     {cpu_use, 100 - Idle}].
