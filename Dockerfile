FROM haskell:8.0.1

RUN apt-get update && \
    apt-get --no-install-recommends install -y \
        git \
        yui-compressor

COPY ./pipes-shell /opt/pipes-shell
COPY ./sitebuilder.cabal /opt/sitebuilder/sitebuilder.cabal

WORKDIR /opt/sitebuilder

RUN cabal update && \
    cabal sandbox init && \
    cabal sandbox add-source /opt/pipes-shell && \
    cabal install --only-dependencies -j4

COPY . /opt/sitebuilder

RUN cabal install -j4

CMD cabal exec sitebuilder
