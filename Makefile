LIBS := src/miniml

PROGS := src/main test/test

all: $(LIBS) $(PROGS)

$(LIBS):
	dune build $(addsuffix .a,$@)

$(PROGS):
	dune build $(addsuffix .exe,$@)

clean:
	dune clean

.PHONY: all clean
