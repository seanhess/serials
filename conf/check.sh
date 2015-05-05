#!/bin/bash

# NOTE: make sure you create env.sh before running! `env > /opt/serials/env.sh`
# restore parent environment
# log to a file
# run the scan
/usr/bin/env - `cat /opt/serials/env.sh` echo "check rdb=$RETHINKDB_PORT_8080_TCP" >> /var/log/serials.log 2>&1
