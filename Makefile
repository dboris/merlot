.PHONY: all build test clean install example

all: build

build:
	dune build

test:
	dune runtest

clean:
	dune clean
	rm -f test/cases/*.core test/cases/*.beam
	rm -f example/1-hello/*.core example/1-hello/*.beam

install:
	dune install

# Run a specific example
example:
	$(MAKE) -C example/1-hello

# Compile a single file (usage: make compile FILE=path/to/file.ml)
compile:
	@if [ -z "$(FILE)" ]; then \
		echo "Usage: make compile FILE=path/to/file.ml"; \
		exit 1; \
	fi
	_build/default/bin/main.exe $(FILE)

# Compile and run a file (usage: make run FILE=path/to/file.ml)
run: compile
	@BASENAME=$$(basename $(FILE) .ml); \
	DIR=$$(dirname $(FILE)); \
	erlc +from_core -o $$DIR $$DIR/$$BASENAME.core && \
	erl -noshell -pa $$DIR -eval "$$BASENAME:main(ok), halt()."

# Development helpers
watch:
	dune build --watch

fmt:
	dune fmt

# Update expected test outputs
update-expected: build
	@for f in test/cases/*.ml; do \
		_build/default/bin/main.exe "$$f"; \
	done
	@for f in test/cases/*.core; do \
		BASENAME=$$(basename "$$f"); \
		cp "$$f" "test/expected/$$BASENAME"; \
	done
	@echo "Updated expected test outputs"
