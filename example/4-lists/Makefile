MERLOT = ../../_build/default/bin/main.exe

.PHONY: all clean run

all: data_pipeline.beam

%.beam: %.ml
	$(MERLOT) $<

run: data_pipeline.beam
	erl -noshell -eval "io:format('Result: ~p~n', [data_pipeline:main(ok)]), halt()."

clean:
	rm -f *.core *.beam *.cmi erl_crash.dump
