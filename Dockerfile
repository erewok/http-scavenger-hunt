FROM debian:stretch-slim as builder

SHELL ["/bin/bash", "-Eeuxo", "pipefail", "-c"]

RUN apt-get update && \
    apt-get install --no-install-recommends -y \
    build-essential \
    libffi-dev \
    libgmp-dev \
    zlib1g-dev \
    curl \
    ca-certificates \
    tcl \
    netbase \
    && curl -sSL "https://get.haskellstack.org/" | sh \
    && rm -rf /var/lib/apt/lists/* \
    && mkdir -p /opt/http-scavenger-hunt/bin

WORKDIR /opt/http-scavenger-hunt/

COPY stack.yaml http-scavenger-hunt.cabal /opt/http-scavenger-hunt/

RUN stack --no-terminal --install-ghc build --only-dependencies

COPY . /opt/http-scavenger-hunt/

RUN stack install

FROM debian:stretch-slim as base_os

RUN apt-get update && \
    apt-get install -y \
    build-essential \
    libffi-dev \
    libgmp-dev \
    curl \
    ca-certificates \
    netbase

COPY --from=builder /root/.local/bin/http-scavenger-hunt/ /opt/http-scavenger-hunt/bin/

WORKDIR /opt/http-scavenger-hunt/

COPY static ./static

RUN adduser --disabled-password --gecos "" scavenger \
    && chown -R scavenger:scavenger /opt/http-scavenger-hunt/

USER scavenger
EXPOSE 8000

CMD ["/opt/http-scavenger-hunt/bin//http-scavenger-hunt"]
