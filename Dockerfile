FROM ubuntu:22.04
RUN mkdir -p /opt/salon \
	&& apt-get update \
	&& apt-get install -y --no-install-recommends build-essential zlib1g-dev libpq-dev libicu-dev \
	&& apt-get install -y ca-certificates && update-ca-certificates \
	&& apt-get clean \
	&& rm -rf /var/lib/apt/lists/*

ARG YESOD_DEMO_LANG=EN
ARG YESOD_MAPBOX_PK
ARG YESOD_STRIPE_PK
ARG YESOD_STRIPE_SK
ARG YESOD_GOOGLE_SITE_VERIFICATION
ARG YESOD_GOOGLE_CLIENT_ID
ARG YESOD_GOOGLE_CLIENT_SECRET
ARG YESOD_YANDEX_VERIFICATION
ARG YESOD_MS_VALIDATE

WORKDIR /opt/salon
COPY salon /opt/salon
COPY static /opt/salon/static
COPY config /opt/salon/config

ENV YESOD_PORT=8080
ENV YESOD_DEMO_LANG=${YESOD_DEMO_LANG}
ENV YESOD_MAPBOX_PK=${YESOD_MAPBOX_PK}
ENV YESOD_STRIPE_PK=${YESOD_STRIPE_PK}
ENV YESOD_STRIPE_SK=${YESOD_STRIPE_SK}
ENV YESOD_GOOGLE_SITE_VERIFICATION=${YESOD_GOOGLE_SITE_VERIFICATION}
ENV YESOD_GOOGLE_CLIENT_ID=${YESOD_GOOGLE_CLIENT_ID}
ENV YESOD_GOOGLE_CLIENT_SECRET=${YESOD_GOOGLE_CLIENT_SECRET}
ENV YESOD_YANDEX_VERIFICATION=${YESOD_YANDEX_VERIFICATION}
ENV YESOD_MS_VALIDATE=${YESOD_MS_VALIDATE}

EXPOSE 8080
CMD ["./salon"]
