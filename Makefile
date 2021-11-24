DUNE=dune
DARGS=$(if $(VERBOSE),--verbose,) $(DUNE_ARGS)

clean:
	$(DUNE) clean $(DARGS)

build:
	$(DUNE) build $(DARGS)

test:
	$(DUNE) runtest $(DARGS)

TARGS=-runner=sequential $(if $(T),-only-test=$(T),) $(if $(LIST_TEST),-list-test,)
test/%:
	$(DUNE) exec test/test_$*.exe $(DARGS) -- $(TARGS)

format:
	$(DUNE) build @fmt --auto-promote $(DARGS)

.PHONY: clean build test format
.DEFAULT: build

