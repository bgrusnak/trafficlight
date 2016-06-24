NODE =  trafficlight
REBAR = ./rebar
CONFIG = app.config
RUN = erl -pa ebin deps/*/ebin -s  ${NODE} -config ${CONFIG} ${ERL_ARGS}


all:
	${REBAR} update-deps compile

quick:
	${REBAR} skip_deps=true compile

compile:
	${REBAR} compile

clean:
	${REBAR} clean

quick_clean:
	${REBAR} skip_deps=true clean

run: quick
	if [ -n "${NODE}" ]; then  ${RUN} -name ${NODE}@`hostname` ; \
	else ${RUN}; \
	fi
