FROM images.home.mtaylor.io/base:latest AS runtime
# Install system dependencies
USER root
RUN apt-get update && apt-get install -y libpq5 zlib1g \
  && apt-get clean && rm -rf /var/lib/apt/lists/* \
  && adduser --system --no-create-home --uid 1000 iam
# Add the built executables
ADD events-mtaylor-io /usr/local/bin/events-mtaylor-io
# Add the migrations
ADD migrations /usr/local/share/events-mtaylor-io/migrations
# Set the user
USER iam
# Set the entrypoint
ENTRYPOINT ["events-mtaylor-io", "server"]
