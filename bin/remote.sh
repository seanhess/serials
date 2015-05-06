#!/bin/bash
set -e

# executed on the server
echo "== REMOTE ============"
pwd

# compile application
echo "-- cabal ----------------"
echo $PATH
cabal sandbox init
cabal update
cabal install --only-dependencies
cabal install

# install upstart script
echo "-- cron ----------------"
cp ./conf/cron.conf /etc/cron.d/serials

# run the application with the database linked
echo "-- run --------------"
cp ./conf/serials.upstart.conf /etc/init/serials.conf

echo "== restarted =================" >> /var/log/serials.log

if ( status serials | grep start ); then
  echo "- restarting"
  restart serials
else
  echo "- starting"
  start serials
fi

