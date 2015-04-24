#!/bin/bash

# Bootstrap server for deployment. Used with vagrant
# USAGE
# > vagrant up

# For installation on a server, follow the steps by hand
# Use ubuntu

##### DEPENDENCIES #############################################################
sudo apt-get update
sudo apt-get install -y wget make libpcre3 libpcre3-dev libcurl4-openssl-dev curl g++


##### LINK DIRECTORIES (Vagrant Only) ##########################################
ln -s /vagrant/rethinkdb_data /var/lib/rethinkdb/instance1/data

###### RETHINKDB ###############################################################
source /etc/lsb-release && echo "deb http://download.rethinkdb.com/apt $DISTRIB_CODENAME main" | sudo tee /etc/apt/sources.list.d/rethinkdb.list
wget -qO- http://download.rethinkdb.com/apt/pubkey.gpg | sudo apt-key add -
sudo apt-get update
sudo apt-get install -y rethinkdb=1.16.3~0trusty

sudo cp /etc/rethinkdb/default.conf.sample /etc/rethinkdb/instances.d/instance1.conf
sudo sed -i.bak 's/\# bind.*/bind=all/' /etc/rethinkdb/instances.d/instance1.conf
service rethinkdb start

####### Install Dependencies ###################################################
sudo apt-add-repository ppa:hvr/ghc -y
sudo apt-get update
sudo apt-get install -y ghc-7.8.3 cabal-install-1.22
echo $'\n\nexport PATH=/opt/ghc/7.8.3/bin:/opt/cabal/1.22/bin:$PATH' >> ~/.bashrc
