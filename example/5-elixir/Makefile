MERLOT = ../../_build/default/bin/main.exe
ELIXIR_EBIN = $(shell elixir -e "IO.puts(:code.lib_dir(:elixir, :ebin))")

.PHONY: all clean run

all: word_stats.beam

%.beam: %.ml
	$(MERLOT) $<

run: word_stats.beam
	erl -noshell -pa "$(ELIXIR_EBIN)" -eval "io:format('Result: ~p~n', [word_stats:main(ok)]), halt()."

clean:
	rm -f *.core *.beam *.cmi erl_crash.dump
