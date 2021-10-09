.PHONY: all bench build check clean doc install publish run test update local_server

all: build

build:
	stack build	

clean:
	stack clean

install: build
	stack install

client: build
	stack exec graphqshell -- --config config/config.yaml

local_client: build_with_server
	stack exec graphqshell -- --config config/config.local.yaml

local_server: build_local_server
	GQL_SERVER_PORT=5050 stack exec demo_server

build_local_server:
	stack install --flag graphqshell:demo-server

build_with_server:
	stack build --flag graphqshell:demo-server

test: 
	stack test
