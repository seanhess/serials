FROM haskell:7.8

WORKDIR /opt/serials

# System dependencies
RUN apt-get update && apt-get install -y libpcre3 libpcre3-dev libcurl4-openssl-dev

# Install the cached dependencies
#ADD ./Serials/deps.cabal    /opt/serials/Serials/
#ADD ./Serials/cabal.config  /opt/serials/Serials/
#RUN cd Serials/ && cabal install --only-dependencies -j4

# Install dependencies (only copy the cabal files over)
ADD ./serials.cabal ./cabal.config /opt/serials/
RUN cabal update && \
    cabal install --only-dependencies

## Install Application
ADD ./server /opt/serials/server
RUN cabal build

# Install Web app
ADD ./web    /opt/serials/web
#RUN cd web && webpack

#Default Command for Container
CMD ["./dist/build/serials/serials"]
