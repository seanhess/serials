#!/bin/bash

# save our environment variables for our poor cron script
# we have to remove OPTS_APT because it has some garbage we can't read
env -u OPTS_APT > env.sh

# cron in the background
cron &

# run the server
./dist/build/serials/serials api
