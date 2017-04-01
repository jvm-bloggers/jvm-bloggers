FROM davidcaste/alpine-java-unlimited-jce:jdk8

MAINTAINER t dziurko at g mail dooot com
VOLUME /tmp
EXPOSE 8080

ENV USER_NAME blogger
ENV APP_HOME /home/$USER_NAME/app

RUN adduser -S $USER_NAME
RUN mkdir $APP_HOME
RUN chown $USER_NAME $APP_HOME

ADD jvm-bloggers-*.jar $APP_HOME/jvm-bloggers.jar
RUN chown $USER_NAME $APP_HOME/jvm-bloggers.jar

USER $USER_NAME
WORKDIR $APP_HOME
RUN sh -c 'touch jvm-bloggers.jar'

ENTRYPOINT ["java","-Djava.security.egd=file:/dev/./urandom","-jar","jvm-bloggers.jar"]
