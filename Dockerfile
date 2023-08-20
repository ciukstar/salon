FROM ubuntu:22.04
RUN mkdir -p /opt/salon \
	&& apt-get update \
	&& apt-get install -y --no-install-recommends build-essential zlib1g-dev libpq-dev libicu-dev \
	&& apt-get clean \
	&& rm -rf /var/lib/apt/lists/*
WORKDIR /opt/salon
COPY salon /opt/salon
COPY static /opt/salon/static
COPY config /opt/salon/config
ENV YESOD_PORT=8080 DEMO_LANG=EN
EXPOSE 8080
CMD ["./salon"]