# JVM Bloggers

    Never miss any blog post from developers around JVM in Poland :)

[![Build Status](https://travis-ci.org/tdziurko/jvm-bloggers.svg?branch=master)](https://travis-ci.org/tdziurko/jvm-bloggers)  [![Coverage Status](https://coveralls.io/repos/tdziurko/jvm-bloggers/badge.svg?branch=master&service=github)](https://coveralls.io/github/tdziurko/jvm-bloggers?branch=master)  [![Join the chat at https://gitter.im/tdziurko/jvm-bloggers](https://badges.gitter.im/tdziurko/jvm-bloggers.svg)](https://gitter.im/tdziurko/jvm-bloggers?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) [![Sputnik](https://sputnik.ci/conf/badge)](https://sputnik.ci/app#/builds/tdziurko/jvm-bloggers)

## Overview
Goal of this project is to gather information about every developer or company blogging about JVM related technologies in Poland, no matter if he is an oldschool 
Java fanboy, fancy Scala lover or niche Gosu expert.

Every Friday at 12am application sends a newsletter with all new blog posts from last 7 days. Currently newsletter is sent to the most of Java User Groups 
in Poland. If you want to have your group or personal e-mail added, please contact me (address below).

I want to promote blogging as the best way to share knowledge and learn from your readers at the same time. And the best way to do that is by helping blog authors to reach wider audience.

##### Your blog/JUG/conference is missing?
Please create a Pull Request adding it to
 
* [bloggers.json](src/main/resources/blogs/bloggers.json) for personal blogs

* [companies.json](src/main/resources/blogs/companies.json) for company blogs

* [videos.json](src/main/resources/blogs/videos.json) for videos from JUGs/Meetups/Conferences

and that's all. Once it is merged, you are on the list!
 
## Roadmap

__Done__

1. Send weekly e-mail about latest blog posts to JUG mailing lists
2. Add technical blogs from Polish companies
3. Add moderation to discard off-topic content
4. Write some admin panel to manage moderation. etc.
5. Publish global RSS feed with all blog posts
6. Add videos from JUGs and conferences in Poland

__Planned__
* Create website with all newsletter (latest and old ones)
* Fetch data about number of comments in articles
* Analyze tweets from developers and highlight those with most RTs, stars and the most active conversations
*  Maybe export the idea to another country? :)
* .... this is a place for __YOUR__ idea :)

## Technical details

Application is written using Java 8, Spring Boot, Liquibase, Akka and JPA, running on PostgreSQL database. Currenly it runs on a server hosted by [SoftwareMill](http://SoftwareMill.com).


## Local development setup: Option A (without Docker)
 
#### Step 1: 

You need a running PostgreSQL instance with database (name: `jvm_bloggers`, user/password: `jvm_bloggers`/`jvm_bloggers`).

#### Step 2:

Modify `spring.datasource.url` in `application-dev.yam` file to point to your local database (it will be `jdbc:postgresql://jvm_bloggers_db:5432/jvm_bloggers` in most cases) 

#### Step 3:

Execute Gradle `bootRun` task:

    ./gradlew  -Djasypt.encryptor.password=<SECRET_PASSWORD> -Dspring.profiles.active=dev bootRun

#### Step 4:

Navigate to [http://localhost:8080/admin](http://localhost:8080/admin) and fill login form with any login and `<SECRET_PASSWORD>` (the password provided in the previous step)

**NOTE:** Admin UI is based on [http://startbootstrap.com/template-overviews/sb-admin-2/](http://startbootstrap.com/template-overviews/sb-admin-2/).

#### Step 5:

To import the project into IDE first execute `./gradlew eclipse` or `./gradlew idea` (depending on your IDE) to generate project files and import them into IDE.

## Local development setup: Option B (with Docker and Docker Compose)

You need to have Docker and Docker Compose installed :)

#### Step 1:

Create your local image by executing `./gradlew clean buildDocker` and then check what is the exact name of created image with `docker images`. You should see something similar to:

    → docker images
    REPOSITORY              TAG                             IMAGE ID            CREATED             SIZE
    tdziurko/jvm-bloggers   0.9.0-20160715-121902-d15c4ed   b783143f6c64        4 days ago          287.8 MB

Alternatively you can use any of published images at https://hub.docker.com/r/tdziurko/jvm-bloggers/tags/
 
#### Step 2:

Put tag of selected image in `jvm-bloggers.sh` file in line:
    
    export JVM_BLOGGERS_CORE_IMAGE_VERSION=0.9.0-20160718-225149-5845d23

#### Step 3:

You can adjust other variables in `jvm-bloggers.sh` script:

* `JVM_BLOGGERS_CORE_PORT` - port on which application will start, default port is 9000

* `JVM_BLOGGERS_DB_PUBLISHED_PORT` - port on which database will be available for external clients e.g. your pgAdmin or something similar, default port for database is 5432
 
* `JVM_BLOGGERS_DB_PATH` - path to database files so they could be mounted as a Docker volume 


### Step 4:

Execute `./jvm-bloggers.sh start` and then open address on which your docker is running e.g. http://localhost:9000/admin and use password `secret` or other defined using `JVM_BLOGGERS_CORE_ENCRYPTOR_PASSWORD` 

You can use `start`, `stop`, `status` or `restart` commands with `jvm-bloggers.sh` script.

## Contributing

Wanna help? Have any problems or questions? Please let me know! You can simply:

* join [Gitter room](https://gitter.im/tdziurko/jvm-bloggers)
* write an e-mail to GMail: jvmbloggers (at) (you know what).com :)
* create an issue or comment on existing one 
* for more details about contributing please read our [Contribution guide](CONTRIBUTING.md)


**Important:** 

1. Before coding please comment on issue that you will be working on so we do not duplicate efforts :)
2. PR with code cleanups are also welcome :)

This is a community driven open source project, so any help is appreciated!

#### Contact

If you need direct contact, you can reach JVM Bloggers project at GMail: jvmbloggers (at) (you know what).(you know what too) :)
