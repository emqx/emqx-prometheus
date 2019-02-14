PROJECT = emqx_statsd
PROJECT_DESCRIPTION = Statsd for EMQ X
PROJECT_VERSION = 3.1

NO_AUTOPATCH = prometheus.erl cuttlefish

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

CUTTLEFISH_SCRIPT = _build/default/lib/cuttlefish/cuttlefish

app.config:: $(CUTTLEFISH_SCRIPT) etc/emqx_statsd.conf
	$(verbose) $(CUTTLEFISH_SCRIPT) -l info -e etc/ -c etc/emqx_statsd.conf -i priv/emqx_statsd.schema -d data

$(CUTTLEFISH_SCRIPT): rebar-deps
	@if [ ! -f cuttlefish ]; then make -C _build/default/lib/cuttlefish; fi

distclean::
	@rm -rf _build cover deps logs log data
	@rm -f rebar.lock compile_commands.json cuttlefish

rebar-deps:
	rebar3 get-deps

rebar-clean:
	@rebar3 clean

rebar-compile: rebar-deps
	rebar3 compile

rebar-ct: app.config
	rebar3 ct

rebar-xref:
	@rebar3 xref

## Below are for version consistency check during erlang.mk and rebar3 dual mode support
none=
space = $(none) $(none)
comma = ,
quote = \"
curly_l = "{"
curly_r = "}"
dep-versions = [$(foreach dep,$(DEPS) $(BUILD_DEPS),$(curly_l)$(dep),$(quote)$(word 3,$(dep_$(dep)))$(quote)$(curly_r)$(comma))[]]

.PHONY: dep-vsn-check
dep-vsn-check:
	$(verbose) erl -noshell -eval \
		"MkVsns = lists:sort(lists:flatten($(dep-versions))), \
		{ok, Conf} = file:consult('rebar.config'), \
		{_, Deps} = lists:keyfind(deps, 1, Conf), \
		F = fun({N, V}) when is_list(V) -> {N, V}; ({N, {git, _, {branch, V}}}) -> {N, V} end, \
		RebarVsns = lists:sort(lists:map(F, Deps)), \
		case {RebarVsns -- MkVsns, MkVsns -- RebarVsns} of \
		  {[], []} -> halt(0); \
		  {Rebar, Mk} -> erlang:error({deps_version_discrepancy, [{rebar, Rebar}, {mk, Mk}]}) \
		end."
