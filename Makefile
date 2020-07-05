LIBS := src/miniml

PROGS := src/main test/expect test/test

all: $(LIBS) $(PROGS) test

$(LIBS):
	dune build $(addsuffix .a,$@)

$(PROGS):
	dune build $(addsuffix .exe,$@)

test: test/expect
	@echo "Running expect tests"
	@test/expect.sh

clean:
	dune clean

.PHONY: all test clean
