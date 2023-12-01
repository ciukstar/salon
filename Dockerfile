FROM ubuntu:22.04
RUN mkdir -p /opt/salon \
	&& apt-get update \
	&& apt-get install -y --no-install-recommends build-essential zlib1g-dev libpq-dev libicu-dev \
	&& apt-get clean \
	&& rm -rf /var/lib/apt/lists/*

ARG YESOD_DEMO_LANG=EN
ARG YESOD_MAPBOX_PK
ARG YESOD_STRIPE_PK
ARG YESOD_STRIPE_SK

WORKDIR /opt/salon
COPY salon /opt/salon
COPY static /opt/salon/static
COPY config /opt/salon/config

ENV YESOD_PORT=8080
ENV YESOD_DEMO_LANG=${YESOD_DEMO_LANG}
ENV YESOD_MAPBOX_PK=${YESOD_MAPBOX_PK}
ENV YESOD_STRIPE_PK=${YESOD_STRIPE_PK}
ENV YESOD_STRIPE_SK=${YESOD_STRIPE_SK}

EXPOSE 8080
CMD ["./salon"]
