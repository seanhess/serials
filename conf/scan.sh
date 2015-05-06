#!/bin/bash

# NOTE: make sure you create env.sh before running! `env > /opt/serials/env.sh`
# restore parent environment
# log to a file
# run the scan
/usr/bin/env - `cat /opt/serials/env.sh` /opt/serials/.cabal-sandbox/bin/serials scan >> /var/log/serials.log 2>&1
