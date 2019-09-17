#!/bin/sh

setupProperties() {
  # Database settings
  export JVM_BLOGGERS_DB_USER=jvm_bloggers
  export JVM_BLOGGERS_DB_PASSWORD=jvm_bloggers
  export JVM_BLOGGERS_DB_NAME=jvm_bloggers
  export JVM_BLOGGERS_DB_PATH="~/jvm-bloggers-postgresql-data"
  export JVM_BLOGGERS_DB_PUBLISHED_PORT=5432

}

start() {
    docker-compose -f docker-compose-local.yml up -d --force-recreate
    echo "Started JVM Bloggers Database"
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
