#!/bin/bash
set -e

# executed on the server
echo "== REMOTE ============"
pwd


echo "-- env -----------------"
if [ -z "$MANDRILL_API_KEY" ]; then
    echo "missing env: MANDRILL_API_KEY"
    exit 1
fi
echo "MANDRILL_API_KEY = $MANDRILL_API_KEY"

# compile application
echo "-- cabal ----------------"
cabal sandbox init
cabal update
cabal install --only-dependencies
cabal install

# install upstart script
echo "-- cron ----------------"
cp ./conf/cron.conf /etc/cron.d/serials

# run the application with the database linked
echo "-- run --------------"
cp -f ./conf/serials.upstart.conf /etc/init/serials.conf

echo "== restarted =================" >> /var/log/serials.log

if ( status serials | grep start ); then
  echo "- restarting"
  restart serials
else
  echo "- starting"
  start serials
fi

