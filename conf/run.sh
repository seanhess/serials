#!/bin/bash

# save our environment variables for our poor cron script
# we have to remove OPTS_APT because it has some garbage we can't read
env -u OPTS_APT > env.sh

# cron in the background
cron & >> /var/log/serials.log 2>&1
echo "Initialized Cron" >> /var/log/serials.log

# run the server
./dist/build/serials/serials api
