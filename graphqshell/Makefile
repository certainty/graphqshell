
.PHONY: all bench build check clean doc install publish run run_local test update

all: build

build:
	stack build	

clean:
	stack clean

install: build
	stack install

run: build
	stack run -- --config config/config.yaml

# run using locally installed configuration
run_local: build
	stack run

test: 
	stack test
