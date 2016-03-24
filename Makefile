.PHONY: all compile check eunit build_plt dialyzer clean shell

all: compile check

compile:
	@rebar3 compile

check:
	@rebar3 eunit
	@rebar3 cover -v

eunit:
	@rebar3 eunit -v

build_plt: horrible-hash.plt

horrible-hash.plt:
	@dialyzer --build_plt -r /usr/local/lib/erlang/lib/erts*/ebin \
		/usr/local/lib/erlang/lib/kernel*/ebin \
		/usr/local/lib/erlang/lib/stdlib*/ebin \
		/usr/local/lib/erlang/lib/crypto*/ebin \
		/usr/local/lib/erlang/lib/compiler*/ebin \
		--output_plt horrible-hash.plt

dialyzer: compile build_plt
	@dialyzer --plt horrible-hash.plt \
	$(PWD)/_build/default/lib/horrible-hash/ebin

clean:
	@rebar3 clean

shell:
	@rebar3 shell
