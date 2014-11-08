compile:
	rebar co

deps:
	rebar get-deps

all: deps compile

killall:
	killall beam.smp

console: all
	ERL_COMPILER_OPTIONS='[{i, "include"}]' erl -pa ebin -pa deps/eleveldb/ebin -pa deps/bear/ebin -pa deps/riak_sysmon/ebin -pa deps/poolboy/ebin -pa deps/pbkdf2/ebin -pa deps/cuttlefish/ebin -pa deps/neotoma/ebin -pa deps/meck/ebin -pa deps/getopt/ebin -pa deps/basho_stats/ebin -pa deps/riak_core/ebin -pa deps/goldrush/ebin -pa deps/riak_ensemble/ebin -pa deps/lager/ebin -pa deps/folsom/ebin -name macbook -s hasler_app

clean:
	rebar clean
	find . -type f -name '*.beam' -delete
	find . -type f -name '*.dump' -delete
