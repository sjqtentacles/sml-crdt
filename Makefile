# sml-crdt build
#
#   make test             build + run tests under MLton (default)
#   make test-poly        build + run tests under Poly/ML
#   make verify-identical byte-compare both compilers' test output
#   make all-tests        both compilers + the byte-identical gate
#   make example          build + run the demo (MLton)
#   make example-poly     build + run the demo (Poly/ML)
#   make clean            remove build artifacts

MLTON      ?= mlton
BIN        := bin
TEST_MLB   := test/sources.mlb
SRCS       := $(shell find lib src test examples -type f \( -name '*.sml' -o -name '*.sig' -o -name '*.mlb' \) 2>/dev/null)

.PHONY: all test poly test-poly verify-identical all-tests example example-poly clean

all: $(BIN)/test-mlton

$(BIN)/test-mlton: $(SRCS) | $(BIN)
	$(MLTON) -output $@ $(TEST_MLB)

test: $(BIN)/test-mlton
	$(BIN)/test-mlton

poly: $(BIN)/test-poly

# Poly/ML has no native .mlb support; tools/polybuild expands the .mlb in
# dependency order, `use`s each source, and exports `main`.
$(BIN)/test-poly: $(SRCS) tools/polybuild | $(BIN)
	sh tools/polybuild -o $@ $(TEST_MLB)

test-poly: $(BIN)/test-poly
	$(BIN)/test-poly

# The dual-compiler contract: both test binaries must print byte-identical
# output. diff exits nonzero (failing the target) on any divergence.
verify-identical: $(BIN)/test-mlton $(BIN)/test-poly
	$(BIN)/test-mlton > $(BIN)/out-mlton.txt
	$(BIN)/test-poly  > $(BIN)/out-poly.txt
	diff $(BIN)/out-mlton.txt $(BIN)/out-poly.txt
	@echo "byte-identical: OK"

all-tests: test test-poly verify-identical

example: $(BIN)/demo
	./$(BIN)/demo

$(BIN)/demo: $(SRCS) | $(BIN)
	$(MLTON) -output $@ examples/sources.mlb

# Demos are top-level scripts (no `main`), so the Poly side runs them via
# use-loading rather than linking a binary.
example-poly:
	sh tools/polybuild -r examples/sources.mlb

$(BIN):
	mkdir -p $(BIN)

clean:
	rm -rf $(BIN)
