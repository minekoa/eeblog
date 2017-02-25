REBAR3=rebar3
PROGRAM_SV=eblogsv
SV_DIR=eblogsv
RELPATH=./$(SV_DIR)/_build/default/rel/$(PROGRAM_SV)

.PHONY: all compile release clean


all: release

compile:
	cd $(SV_DIR);$(REBAR3) compile
	cd $(SV_DIR);$(REBAR3) dialyzer

release: compile
	cd $(SV_DIR);$(REBAR3) release

clean:
	cd $(SV_DIR);$(REBAR3) clean

run: release
	$(RELPATH)/bin/$(PROGRAM_SV) console
