REBAR3=rebar3
ELM_MAKE=elm-make
ELM_PACKAGE=elm-package

PROGRAM_SV=eblogsv
SV_DIR=server
SPA_SRC_DIR=spa
SPA_OUT_DIR=$(SV_DIR)/priv
RELPATH=./$(SV_DIR)/_build/default/rel/$(PROGRAM_SV)

.PHONY: all env compile release clean


all: release

env:
	elm-install

$(SPA_OUT_DIR)/Main.js: $(SPA_SRC_DIR)/Main.elm
	$(ELM_MAKE) $(SPA_SRC_DIR)/Main.elm --output $(SPA_OUT_DIR)/Main.js

compile: $(SPA_OUT_DIR)/Main.js
	cd $(SV_DIR);$(REBAR3) compile
	cd $(SV_DIR);$(REBAR3) dialyzer

release: compile
	cd $(SV_DIR);$(REBAR3) release

clean:
	cd $(SV_DIR);$(REBAR3) clean
	rm $(SPA_OUT_DIR)/Main.js

run: release
	$(RELPATH)/bin/$(PROGRAM_SV) console
