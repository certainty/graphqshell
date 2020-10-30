COLOR ?= always # Valid COLOR options: {always, auto, never}
CARGO = cargo --color $(COLOR)

.PHONY: build check clean doc install publish run test update

.DEFAULT_GOAL := build

build:
	$(CARGO) build

run:
	$(CARGO) run

check:
	$(CARGO) check

clean:
	$(CARGO) clean

doc:
	$(CARGO) doc

publish:
	$(CARGO) publish

test: build
	$(CARGO) test -- --nocapture

update:
	$(CARGO) update

