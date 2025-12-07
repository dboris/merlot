MERLOT = ../../_build/default/bin/main.exe

.PHONY: all clean run

all: counter.beam

%.beam: %.ml
	$(MERLOT) $<

run: counter.beam
	erl -noshell -eval "io:format('Result: ~p~n', [counter:main(ok)]), halt()."

clean:
	rm -f *.core *.beam *.cmi erl_crash.dump
