#!/bin/sh

setupProperties() {
  # Database settings
  export JVM_BLOGGERS_DB_USER=jvm_bloggers
  export JVM_BLOGGERS_DB_PASSWORD=jvm_bloggers
  export JVM_BLOGGERS_DB_NAME=jvm_bloggers
  export JVM_BLOGGERS_DB_PATH="${HOME}/postgresql-data/"
  export JVM_BLOGGERS_DB_PUBLISHED_PORT=5432

  # Reverse proxy settings
  export JVM_BLOGGERS_CADDY_PATH="${HOME}/caddy-data"

  # Core Application settings:
  export JVM_BLOGGERS_CORE_IMAGE_VERSION=2.0.0-20200202-180146-11c524a7
  export JVM_BLOGGERS_CORE_SPRING_PROFILES=dev
  export JVM_BLOGGERS_CORE_ENCRYPTOR_PASSWORD=secret

  export DATADOG_API_KEY=api_key
}

start() {
    docker-compose up -d
    echo "Started JVM Bloggers"
}

stop() {
    docker-compose down
}

status() {
    echo "** ** ** ** ** ** ** ** ** ** ** ** "
    echo "-- From docker-compose:"
    docker-compose ps
    echo "** ** ** ** ** ** ** ** ** ** ** ** "
    echo "-- From docker itself:"
    docker ps
    echo "** ** ** ** ** ** ** ** ** ** ** ** "
}

logs() {
    docker-compose logs
}

setupProperties

case "$1" in
  start)
    start
    ;;
  stop)
    stop
    ;;
  status)
    status
    ;;
  logs)
    logs
    ;;
  restart)
    stop
    start
    ;;
  *)
    echo "Usage: $0 {start|stop|restart|status}"
esac
