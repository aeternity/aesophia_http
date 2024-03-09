FROM aeternity/builder:bionic-otp23 as builder

# Add rebar configuration and fetch dependencies
ADD ./Makefile ./rebar* /app/
RUN cd /app && make get-deps
# Add the whole project and compile it.
ADD . /app
RUN cd /app && make prod-build

# Put aeternity node in second stage container
FROM ubuntu:18.04

# OpenSSL is shared lib dependency
RUN apt-get -qq update && apt-get -qq -y install libssl1.0.0 curl libsodium23 \
    && ldconfig \
    && rm -rf /var/lib/apt/lists/*

# Deploy application code from builder container
COPY --from=builder /app/_build/prod/rel/aesophia_http /home/aeternity/aesophia_http

# Aeternity app won't run as root for security reasons
RUN useradd --shell /bin/bash aeternity \
    && chown -R aeternity:aeternity /home/aeternity

# Switch to non-root user
USER aeternity
ENV SHELL /bin/bash

WORKDIR /home/aeternity/aesophia_http

# Erl handle SIGQUIT instead of the default SIGINT
STOPSIGNAL SIGQUIT

EXPOSE 3080

COPY ./docker/entrypoint.sh /docker-entrypoint.sh
COPY ./docker/healthcheck.sh /healthcheck.sh

ENTRYPOINT ["/docker-entrypoint.sh"]
HEALTHCHECK --timeout=3s --retries=6 CMD /healthcheck.sh
