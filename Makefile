PROJECT = emqx_statsd
PROJECT_DESCRIPTION = Statsd for EMQ X
PROJECT_VERSION = 3.0

NO_AUTOPATCH = prometheus.erl

DEPS = prometheus
dep_prometheus = git https://github.com/turtleDeng/prometheus.erl v3.1.1

BUILD_DEPS = emqx cuttlefish
dep_emqx = git https://github.com/emqtt/emqttd emqx30
dep_cuttlefish = git https://github.com/emqtt/cuttlefish develop

ERLC_OPTS += +debug_info

TEST_ERLC_OPTS += +debug_info

COVER = true

include erlang.mk

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emqx_statsd.conf -i priv/emqx_statsd.schema -d data
