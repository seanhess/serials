#!/bin/bash

# NOTE: make sure you create env.sh before running! `env > /opt/serials/env.sh`
# restore parent environment
# log to a file
# run the scan
export RETHINKDB_PORT_28015_TCP=tcp://localhost:28015
export ENDPOINT=http://serials.orbit.al
/usr/bin/env - `cat /etc/environment` /opt/serials/.cabal-sandbox/bin/serials scan >> /var/log/serials.log 2>&1
