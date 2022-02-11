COLOR ?= always # Valid COLOR options: {always, auto, never}
CARGO = cargo --color $(COLOR)

.PHONY: all bench build check clean doc install run debug_run test update

all: build


dev:
	$(CARGO) tauri dev

bench:
	$(CARGO) bench

build:
	$(CARGO) tauri build

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
