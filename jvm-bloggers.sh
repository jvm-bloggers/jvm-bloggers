#!/bin/sh

setupProperties() {
  # Database settings
  export JVM_BLOGGERS_DB_USER=jvm_bloggers
  export JVM_BLOGGERS_DB_PASSWORD=jvm_bloggers
  export JVM_BLOGGERS_DB_NAME=jvm_bloggers
  export JVM_BLOGGERS_DB_PATH="~/postgresql-data/"
  export JVM_BLOGGERS_DB_PUBLISHED_PORT=5432

  # Core Application settings:
  export JVM_BLOGGERS_CORE_IMAGE_VERSION=1.0.0-20161202-230241-fb9ffbd
  export JVM_BLOGGERS_CORE_SPRING_PROFILES=dev
  export JVM_BLOGGERS_CORE_ENCRYPTOR_PASSWORD=secret
  export JVM_BLOGGERS_CORE_PORT=9000

  #Twitter settings
  export JVM_BLOGGERS_TWITTER_CLIENT_PORT=9001
  #Credentials for fake test account -> https://twitter.com/JvmTest
  export JVM_BLOGGERS_TWITTER_CONSUMER_KEY="Dsi4rXqresiqm1YE6ZYmKZtra"
  export JVM_BLOGGERS_TWITTER_CONSUMER_SECRET="WcWU68jIct0LkLoOSlfWIip16qVlTsu5sp54lg7emhvNHA1gtq"
  export JVM_BLOGGERS_TWITTER_ACCESS_TOKEN="768504700031426562-SQNcsArlg0937WqVPMnxCyFGzJUXA4D"
  export JVM_BLOGGERS_TWITTER_ACCESS_SECRET="cT1B9xaw15Y4kAJWh86WZMEdapgmu60EIGwu4tORtxuSH"

  #Kafka settings:
  export JVM_BLOGGERS_KAFKA_VERSION=0.10.0.1
  export JVM_BLOGGERS_KAFKA_PORT=9092

  #Zookeeper settings
  export JVM_BLOGGERS_ZOOKEEPER_VERSION=3.4.6
  export JVM_BLOGGERS_ZOOKEEPER_PORT=2181
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