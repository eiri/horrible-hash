.PHONY: all compile check eunit build_plt dialyzer clean shell

all: compile check

compile:
	@rebar3 compile

check:
	@rebar3 eunit -c
	@rebar3 cover -v

eunit:
	@rebar3 eunit -v

dialyzer: compile
	@rebar3 dialyzer

clean:
	@rebar3 clean

shell:
	@epmd -daemon
	@rebar3 shell --name hhash
