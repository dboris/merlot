MERLOT = ../../_build/default/bin/main.exe

.PHONY: all clean test

all: Elixir.Person.beam Merlot.Consumer.beam

# Compile Merlot module that exports an Elixir struct
Elixir.Person.beam: person.ml
	$(MERLOT) $<

# Compile Merlot module that imports Elixir structs
Merlot.Consumer.beam: consumer.ml
	$(MERLOT) $<

# Run the Elixir demo
test: Elixir.Person.beam
	elixir demo.exs

# Run the Merlot consumer (demonstrates importing Elixir structs)
run-consumer: Merlot.Consumer.beam Elixir.Person.beam
	erl -noshell -pa . -eval " \
		Person = 'Elixir.Person':'new_'(<<\"Alice\">>, 30), \
		io:format(\"Person: ~p~n\", [Person]), \
		Name = 'Merlot.Consumer':greet(Person), \
		io:format(\"Greeting: ~p~n\", [Name]), \
		IsSenior = 'Merlot.Consumer':is_senior(Person), \
		io:format(\"Is senior: ~p~n\", [IsSenior]), \
		Updated = 'Merlot.Consumer':with_age(Person, 70), \
		io:format(\"Updated person: ~p~n\", [Updated]), \
		IsSeniorNow = 'Merlot.Consumer':is_senior(Updated), \
		io:format(\"Is senior now: ~p~n\", [IsSeniorNow]), \
		halt()."

# Show the generated Core Erlang (for debugging)
core: person.ml consumer.ml
	$(MERLOT) person.ml --emit-core
	$(MERLOT) consumer.ml --emit-core
	@echo "=== Generated Core Erlang ==="
	@cat *.core

clean:
	rm -f *.beam *.core erl_crash.dump
