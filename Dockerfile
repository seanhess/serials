FROM haskell:7.8

WORKDIR /opt/serials

# System dependencies
RUN apt-get update && apt-get install -y libpcre3 libpcre3-dev libcurl4-openssl-dev cron vim

# Copy only the freeze file, install cached dependencies
ADD ./cabal.config /opt/serials/
RUN cabal update && \
    cabal install tagsoup text scalpel containers network-uri monad-loops wreq lens bytestring parsec utf8-string tagsoup xml feed regex-pcre aeson network wai wai-extra wai-cors warp scotty servant-server rethinkdb transformers either unordered-containers mtl http-types safe hashable resource-pool time

# Add New Dependencies
ADD ./serials.cabal /opt/serials/
RUN cabal update && \
    cabal install --only-dependencies

## Install Application
ADD ./server /opt/serials/server
RUN cabal build

# Install Web app. Don't run webpack, just make sure to run it before building
# see bin/build
ADD ./web    /opt/serials/web

# Add configuration files
ADD ./conf/  /opt/serials/conf/

# add cron
# https://phusion.github.io/baseimage-docker/
RUN crontab ./conf/cron.conf

CMD ["/bin/bash", "./conf/run.sh"]

