.PHONY: all compile clean test dialyzer

REBAR ?= rebar3

all: compile

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

test:
	@$(REBAR) ct

dialyzer:
	@$(REBAR) dialyzer
