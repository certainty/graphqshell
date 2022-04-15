.PHONY: all bench build check clean doc install run test 

all: build

build:
	stack build	

clean:
	stack clean

install: build
	stack install

run: build
	stack run -- -c config/default.yaml

test: 
	stack test
