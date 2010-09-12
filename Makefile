
REBAR=$(shell which rebar || echo ./rebar)

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

