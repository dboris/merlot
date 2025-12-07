# Actor Example Makefile

MERLOT = ../../_build/default/bin/main.exe
PRIV = ../../priv

.PHONY: all clean run

all: counter.beam merlot_actor.beam

counter.beam: counter.ml
	$(MERLOT) counter.ml

merlot_actor.beam: $(PRIV)/merlot_actor.erl
	erlc $(PRIV)/merlot_actor.erl

run: counter.beam merlot_actor.beam
	erl -noshell -eval "io:format(\"Result: ~p~n\", [counter:main(ok)]), halt()."

clean:
	rm -f *.core *.beam *.cmi erl_crash.dump
