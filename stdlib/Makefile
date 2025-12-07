# Stdlib compilation to BEAM
#
# Module naming strategy:
# - All modules are prefixed with "Merlot." to prevent shadowing Erlang/OTP modules
# - Output files are named Merlot.Module.core and Merlot.Module.beam
# - This follows Elixir's convention (Elixir.Module) for namespace isolation
#
# See docs/TODO-stdlib.md for full details.

MERLOT = ../_build/default/bin/main.exe

# Safe modules that don't conflict with Erlang or OCaml stdlib
SAFE_MODULES = actor enum keyword map option process result supervisor

# Generate targets with Merlot. prefix
PREFIXED_MODULES = $(addprefix Merlot.,$(shell echo $(SAFE_MODULES) | sed 's/\b\([a-z]\)/\u\1/g'))
BEAM_FILES = $(addsuffix .beam,$(PREFIXED_MODULES))
CORE_FILES = $(addsuffix .core,$(PREFIXED_MODULES))

.PHONY: all clean beam core

all: beam

beam: $(BEAM_FILES)
	@echo "Compiled $(words $(BEAM_FILES)) stdlib modules to BEAM"

core: $(CORE_FILES)
	@echo "Generated $(words $(CORE_FILES)) Core Erlang files"

# Compile each .ml file to .core (output filename matches module name)
Merlot.Actor.core: actor.ml
	cd .. && _build/default/bin/main.exe --emit-core stdlib/$<

Merlot.Enum.core: enum.ml
	cd .. && _build/default/bin/main.exe --emit-core stdlib/$<

Merlot.Keyword.core: keyword.ml
	cd .. && _build/default/bin/main.exe --emit-core stdlib/$<

Merlot.Map.core: map.ml
	cd .. && _build/default/bin/main.exe --emit-core stdlib/$<

Merlot.Option.core: option.ml
	cd .. && _build/default/bin/main.exe --emit-core stdlib/$<

Merlot.Process.core: process.ml
	cd .. && _build/default/bin/main.exe --emit-core stdlib/$<

Merlot.Result.core: result.ml
	cd .. && _build/default/bin/main.exe --emit-core stdlib/$<

Merlot.Supervisor.core: supervisor.ml
	cd .. && _build/default/bin/main.exe --emit-core stdlib/$<

# Pattern rule: compile .core to .beam
%.beam: %.core
	erlc +from_core $<

clean:
	rm -f *.core *.beam Merlot.*.core Merlot.*.beam

# Danger: These modules MUST NOT be compiled to BEAM
# Uncommenting these would shadow Erlang's built-in modules
# lists.beam: FORBIDDEN - shadows erlang:lists
# io.beam: FORBIDDEN - shadows erlang:io
# erlang.beam: FORBIDDEN - shadows erlang:erlang
# timer.beam: FORBIDDEN - shadows erlang:timer
