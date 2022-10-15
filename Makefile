all: test

test: test/test.exe test/expect.exe
	@dune exec test/test.exe 2> /dev/null
	@echo "Running expect tests"
	@test/expect.sh

%.exe: %.ml
	dune build $@

clean:
	dune clean

.PHONY: all test clean
