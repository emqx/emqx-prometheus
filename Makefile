PROJECT = emqx_statsd
PROJECT_DESCRIPTION = Statsd for EMQ X
PROJECT_VERSION = 3.0

NO_AUTOPATCH = prometheus.erl

DEPS = prometheus
dep_prometheus = git-emqx https://github.com/turtleDeng/prometheus.erl v3.1.1

BUILD_DEPS = emqx cuttlefish
dep_emqx = git-emqx https://github.com/emqx/emqx emqx30
dep_cuttlefish = git-emqx https://github.com/emqx/cuttlefish develop

ERLC_OPTS += +debug_info

TEST_ERLC_OPTS += +debug_info

COVER = true

define dep_fetch_git-emqx
	git clone -q --depth 1 -b $(call dep_commit,$(1)) -- $(call dep_repo,$(1)) $(DEPS_DIR)/$(call dep_name,$(1)) > /dev/null 2>&1; \
	cd $(DEPS_DIR)/$(call dep_name,$(1));
endef

include erlang.mk

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emqx_statsd.conf -i priv/emqx_statsd.schema -d data
