MERLOT = ../../_build/default/bin/main.exe

.PHONY: all clean run

all: greeting.beam

%.beam: %.ml
	$(MERLOT) $<

run: greeting.beam
	erl -noshell -eval "greeting:main(ok), halt()."

clean:
	rm -f *.core *.beam *.cmi erl_crash.dump
