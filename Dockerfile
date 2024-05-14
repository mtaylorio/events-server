FROM alpine:3.19
# Install system dependencies
USER root
RUN apk add --no-cache gmp libpq zlib \
  && adduser --system --no-create-home --uid 1000 iam
# Add the built executables
ADD events-mtaylor-io /usr/local/bin/events-mtaylor-io
# Add the migrations
ADD migrations /usr/local/share/events-mtaylor-io/migrations
# Set the user
USER iam
# Set the entrypoint
ENTRYPOINT ["events-mtaylor-io", "server"]
