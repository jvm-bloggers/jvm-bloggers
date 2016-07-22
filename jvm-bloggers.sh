#!/bin/sh

setupProperties() {
  # Database settings
  export JVM_BLOGGERS_DB_USER=jvm_bloggers
  export JVM_BLOGGERS_DB_PASSWORD=jvm_bloggers
  export JVM_BLOGGERS_DB_NAME=jvm_bloggers
  export JVM_BLOGGERS_DB_PATH="~/postgresql-data/"
  export JVM_BLOGGERS_DB_PUBLISHED_PORT=5432

  # Core Application settings:
  export JVM_BLOGGERS_CORE_IMAGE_VERSION=0.9.0-20160722-221143-ad56f2c
  export JVM_BLOGGERS_CORE_SPRING_PROFILES=dev
  export JVM_BLOGGERS_CORE_ENCRYPTOR_PASSWORD=secret
  export JVM_BLOGGERS_CORE_PORT=9000
}

start() {
    docker-compose up
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
  retart)
    stop
    start
    ;;
  *)
    echo "Usage: $0 {start|stop|restart|status}"
esac