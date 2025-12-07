# Supervisor Example Makefile

MERLOT = ../../_build/default/bin/main.exe
PRIV = ../../priv

.PHONY: all clean run

all: workers.beam merlot_actor.beam merlot_supervisor.beam

workers.beam: workers.ml
	$(MERLOT) workers.ml

merlot_actor.beam: $(PRIV)/merlot_actor.erl
	erlc $(PRIV)/merlot_actor.erl

merlot_supervisor.beam: $(PRIV)/merlot_supervisor.erl
	erlc $(PRIV)/merlot_supervisor.erl

run: workers.beam merlot_actor.beam merlot_supervisor.beam
	erl -noshell -eval "io:format(\"Result: ~p~n\", [workers:main(ok)]), halt()."

clean:
	rm -f *.core *.beam *.cmi erl_crash.dump
