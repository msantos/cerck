REBAR ?= rebar3

all: compile

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

.PHONY: test dialyzer clean

test:
	@$(REBAR) ct

dialyzer:
	@$(REBAR) dialyzer
