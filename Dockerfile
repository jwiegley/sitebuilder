FROM haskell:8.0.2

RUN apt-get update && \
    apt-get --no-install-recommends install -y \
        git \
        yui-compressor

COPY ./sitebuilder.cabal /opt/sitebuilder/sitebuilder.cabal

WORKDIR /opt/sitebuilder

RUN cabal update && \
    cabal sandbox init && \
    cabal install --only-dependencies -j4

COPY . /opt/sitebuilder

RUN cabal install -j4

RUN mkdir /tmp/site
WORKDIR /tmp/site

CMD ["cabal", "--sandbox-config-file=/opt/sitebuilder/cabal.sandbox.config", "exec", "sitebuilder"]
