FROM haskell:7.8

WORKDIR /opt/serials

# System dependencies
RUN apt-get update && apt-get install -y libpcre3 libpcre3-dev libcurl4-openssl-dev cron vim rsyslog

# Add New Dependencies
ADD ./serials.cabal ./cabal.config /opt/serials/
RUN cabal update && \
    cabal install --only-dependencies

# install config files and scripts
ADD ./conf/  /opt/serials/conf/

## Install Haskell Application
ADD ./server /opt/serials/server
RUN cabal install

# Install Web app. Don't run webpack, just make sure to run it before building
# see bin/build
ADD ./web    /opt/serials/web

CMD ["/root/.cabal/bin/serials api"]

