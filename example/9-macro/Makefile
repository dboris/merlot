.PHONY: all clean test

MERLOT = ../../_build/default/bin/main.exe

all: debug.beam

debug.beam: debug.ml
	$(MERLOT) debug.ml

test: debug.beam
	@erl -noshell -eval "io:format(\"Result: ~p~n\", ['Merlot.Debug':example(ok)]), halt()."

clean:
	rm -f *.core *.beam *.cmi erl_crash.dump
