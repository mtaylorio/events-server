FROM images.home.mtaylor.io/haskell:latest AS build
# Install system dependencies
USER root
RUN apt-get update && apt-get install -y libpq-dev zlib1g-dev \
  && apt-get clean && rm -rf /var/lib/apt/lists/*
# Add metadata files and build dependencies
USER build
RUN stack install --system-ghc --resolver lts-21.25 \
  aeson base64 bytestring case-insensitive ed25519 email-validate \
  hasql hasql-pool hasql-th hasql-transaction http-client http-client-tls \
  http-types lens mtl optparse-applicative servant-client servant-server \
  stm text uuid vector wai warp
ADD --chown=build:build . .
RUN stack build --system-ghc --copy-bins


FROM images.home.mtaylor.io/base:latest AS runtime
# Install system dependencies
USER root
RUN apt-get update && apt-get install -y libpq5 zlib1g \
  && apt-get clean && rm -rf /var/lib/apt/lists/* \
  && adduser --system --no-create-home --uid 1000 iam
# Copy the built executables
COPY --from=build /build/.local/bin/events-mtaylor-io /usr/local/bin/events-mtaylor-io
# Add the migrations
ADD migrations /usr/local/share/events-mtaylor-io/migrations
# Set the user
USER iam
# Set the entrypoint
ENTRYPOINT ["events-mtaylor-io", "server"]
