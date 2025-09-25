FROM debian:stable AS build

# GHC version (override with --build-arg GHC_VERSION=9.4.8)
ARG GHC_VERSION=9.4.8
ENV GHC_VERSION=${GHC_VERSION} \
    PATH="/root/.ghcup/bin:$PATH"

# Install dependencies for GHC, Cabal, and SQLite
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    build-essential \
    binutils \
    binutils-gold \
    xz-utils \
    git \
    bash curl ca-certificates \
    libffi-dev libgmp-dev libffi8 \
    libgmp10 libncurses-dev libncurses6 libtinfo6 \
    libsqlite3-dev pkg-config \
    && rm -rf /var/lib/apt/lists/*

# Debug check for linker
RUN which ld && ld --version && which ld.gold && ld.gold --version

# Install GHC and Cabal via GHCup
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org \
    | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 sh && \
    ghcup install ghc --set ${GHC_VERSION} && \
    ghcup install cabal --set latest

# Copy project files
COPY src /workdir/src  
COPY mbt-server.cabal CHANGELOG.md /workdir/
WORKDIR /workdir

# Update Cabal package list and build
RUN cabal update && \
    cabal install --installdir=/workdir/

# Second stage: HTTP server
FROM httpd

FROM httpd

COPY ./my-httpd.conf /usr/local/apache2/conf/httpd.conf

COPY ./html/index.html /usr/local/apache2/htdocs/
COPY ./src/Parameters /usr/local/apache2/cgi-bin/Parameters

COPY ./html/tutor.css /usr/local/apache2/htdocs/
COPY ./html/media /usr/local/apache2/htdocs/media

COPY --from=build "/workdir/mbt-server" /usr/local/apache2/cgi-bin/mbt-server.cgi
