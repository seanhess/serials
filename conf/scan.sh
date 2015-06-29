#!/bin/bash

# NOTE: make sure you create env.sh before running! `env > /opt/serials/env.sh`
# restore parent environment
# log to a file
# run the scan

# Make sure to put the following in /etc/environment
# export RETHINKDB_PORT_28015_TCP=tcp://localhost:28015
# export ENDPOINT=http://webfiction.co

/usr/bin/env - `cat /etc/environment` cd /opt/serials/ && /usr/bin/stack exec serials scan >> /var/log/serials.log 2>&1
