#!/bin/bash

# As this script might be used as docker health check it should exit with either 0/1

EXTERNAL_ADDRESS=${EXTERNAL_ADDRESS:-localhost:3080}

curl -s -f -S -o /dev/null --retry 6 http://${EXTERNAL_ADDRESS}/version || exit 1

