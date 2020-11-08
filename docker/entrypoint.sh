#!/usr/bin/env bash 
set -euo pipefail

alias demo="/root/.local/bin/graphqshell-exe --api http://demo_server:$GQL_SERVER_PORT/graphql"
echo "Just execute 'demo' to run the demo"

