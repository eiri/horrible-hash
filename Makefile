.PHONY: all compile clean shell

all: compile check

compile:
	@rebar3 compile

check:
	@rebar3 eunit -v

clean:
	@rebar3 clean

shell:
	@rebar3 shell
