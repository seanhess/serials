#!/bin/bash

# NOTE! this takes FOREVER. The vagrant box only runs with 1 core so cabal install takes a million years

# Bootstrap server for deployment. Used with vagrant
# USAGE
# > vagrant up

# For installation on a server, follow the steps by hand
# Use ubuntu

##### DEPENDENCIES #############################################################
sudo apt-get update
sudo apt-get install -y wget make libpcre3 libpcre3-dev libcurl4-openssl-dev curl g++


###### RETHINKDB ###############################################################
source /etc/lsb-release && echo "deb http://download.rethinkdb.com/apt $DISTRIB_CODENAME main" | sudo tee /etc/apt/sources.list.d/rethinkdb.list
wget -qO- http://download.rethinkdb.com/apt/pubkey.gpg | sudo apt-key add -
sudo apt-get update
sudo apt-get install -y rethinkdb=1.16.3~0trusty

sudo cp /etc/rethinkdb/default.conf.sample /etc/rethinkdb/instances.d/instance1.conf
sudo sed -i.bak 's/\# bind.*/bind=all/' /etc/rethinkdb/instances.d/instance1.conf
service rethinkdb start

##### LINK DIRECTORIES (Vagrant Only) ##########################################
ln -s /vagrant/rethinkdb_data /var/lib/rethinkdb/instance1/data

####### Install Dependencies ###################################################
sudo apt-add-repository ppa:hvr/ghc -y
sudo apt-get update
sudo apt-get install -y ghc-7.8.3 cabal-install-1.22
echo $'\n\nexport PATH=/opt/ghc/7.8.3/bin:/opt/cabal/1.22/bin:$PATH' >> /home/vagrant/.bashrc

####### Node ########################################
curl -sL https://deb.nodesource.com/setup | sudo bash -
sudo apt-get install -y nodejs
npm install webpack

###### it needs extra memory to complete the install
dd if=/dev/zero of=/tmp/swap bs=1M count=1024
mkswap /tmp/swap
swapon /tmp/swap

####### Init Project ###########################################################
cd /vagrant
cabal sandbox init
cabal update
cabal install --only-dependencies


