.PHONY: all clean run test

MERLOT ?= merlot

all: expose.beam

expose.beam: expose.ml
	$(MERLOT) $<

run: all
	@erl -noshell -eval "'Merlot.Expose':main(ok), halt()."

test: run

clean:
	rm -f *.beam *.core erl_crash.dump
