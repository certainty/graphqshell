COLOR ?= always # Valid COLOR options: {always, auto, never}
CARGO = cargo --color $(COLOR)

.PHONY: all bench build check clean doc install run debug_run test update

all: build

run:
	$(CARGO) run --bin graphqshell -- --config config/dev.toml

debug_run:
	$(CARGO) run --features debug --bin graphqshell -- --config config/dev.toml

bench:
	$(CARGO) bench

build:
	$(CARGO) build

check:
	$(CARGO) check

clean:
	$(CARGO) clean

doc:
	$(CARGO) doc

install: build
	$(CARGO) install

update:
	$(CARGO) update

test: build
	$(CARGO) test -- --nocapture
