FROM haskell:7.8

WORKDIR /opt/serials

# System dependencies
RUN apt-get update && apt-get install -y libpcre3 libpcre3-dev libcurl4-openssl-dev

RUN cabal update

# Install the cached dependencies
ADD ./Serials/deps.cabal    /opt/serials/Serials/
ADD ./Serials/cabal.config  /opt/serials/Serials/
RUN cd Serials/ && cabal install --only-dependencies -j4

# Now install new ones!
ADD ./serials.cabal /opt/serials/
ADD ./cabal.config  /opt/serials/
RUN cabal install --only-dependencies -j4

# Add and Install Application Code
ADD ./Serials /opt/serials/Serials/
ADD ./*.hs    /opt/serials/

RUN cabal build serials

# Build the front-end, run webpack before building docker
ADD ./*.md    /opt/serials/
ADD ./*.json  /opt/serials/
ADD ./web     /opt/serials/web/

# Default Command for Container
CMD ["./dist/build/serials/serials"]
