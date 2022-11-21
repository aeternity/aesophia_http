FROM aeternity/builder:bionic-otp23 as builder

RUN curl https://github.com/mikefarah/yq/releases/download/v4.30.4/yq_linux_amd64 -Lo /usr/bin/yq \
    && chmod +x /usr/bin/yq

# Add the whole project and compile it.
ADD . /app
RUN cd /app && make prod-build

# Put aeternity node in second stage container
FROM ubuntu:18.04

# Deploy application code from builder container
COPY --from=builder /app/_build/prod/rel/aesophia_http /home/aeternity/aesophia_http

# OpenSSL is shared lib dependency
RUN apt-get -qq update && apt-get -qq -y install libssl1.0.0 curl libsodium23 \
    && ldconfig \
    && rm -rf /var/lib/apt/lists/*

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
HEALTHCHECK --timeout=3s CMD /healthcheck.sh

