FROM haskell:9.4-slim

ARG ORMOLU_VERSION=0.8.0.0
# NB! The version of ormolu here should correspond to whatever version
# the current haskell-actions/run-ormolu is using in
# .github/workflows/main.yml
#
# See also
#   https://github.com/haskell-actions/run-ormolu/releases
#   https://github.com/tweag/ormolu/releases/

RUN useradd -m alis

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      wget unzip
USER alis
WORKDIR /home/alis
RUN mkdir -p .local/bin
ENV PATH="$PATH:/home/alis/.local/bin"
RUN wget --no-verbose -O ormolu.zip \
  "https://github.com/tweag/ormolu/releases/download/$ORMOLU_VERSION/ormolu-x86_64-linux.zip"
RUN unzip ormolu.zip -d .local/bin/ && rm ormolu.zip
