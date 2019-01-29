PROJECT = emqx_statsd
PROJECT_DESCRIPTION = Statsd for EMQ X
PROJECT_VERSION = 3.0

NO_AUTOPATCH = prometheus.erl

DEPS = prometheus
dep_prometheus = git-emqx https://github.com/turtleDeng/prometheus.erl v3.1.1

BUILD_DEPS = emqx cuttlefish
dep_emqx = git-emqx https://github.com/emqx/emqx testing
dep_cuttlefish = git-emqx https://github.com/emqx/cuttlefish v2.2.1

ERLC_OPTS += +debug_info

TEST_ERLC_OPTS += +debug_info

COVER = true

$(shell [ -f erlang.mk ] || curl -s -o erlang.mk https://raw.githubusercontent.com/emqx/erlmk/master/erlang.mk)
include erlang.mk

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emqx_statsd.conf -i priv/emqx_statsd.schema -d data
