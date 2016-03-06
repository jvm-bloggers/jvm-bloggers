# JVM Bloggers

    Never miss any blog post from developers around JVM in Poland :)

[![Build Status](https://travis-ci.org/tdziurko/jvm-bloggers.svg?branch=master)](https://travis-ci.org/tdziurko/jvm-bloggers)  [![Coverage Status](https://coveralls.io/repos/tdziurko/jvm-bloggers/badge.svg?branch=master&service=github)](https://coveralls.io/github/tdziurko/jvm-bloggers?branch=master)  [![Join the chat at https://gitter.im/tdziurko/jvm-bloggers](https://badges.gitter.im/tdziurko/jvm-bloggers.svg)](https://gitter.im/tdziurko/jvm-bloggers?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

## Overview
Goal of this project is to gather information about every developer or company blogging about JVM related technologies in Poland, no matter if he is an oldschool 
Java fanboy, fancy Scala lover or niche Gosu expert.

Every Friday at 12am application sends a newsletter with all new blog posts from last 7 days. Currently newsletter is sent to the most of Java User Groups 
in Poland. If you want to have your group or personal e-mail added, please contact me (address below).

I want to promote blogging as the best way to share knowledge and learn from your readers at the same time. And the best way to do that is by helping blog authors to reach wider audience.

##### Your blog is missing? 
Please create a Pull Request adding it to
 
* [bloggers.json](https://github.com/tdziurko/jvm-bloggers/blob/master/bloggers.json) for personal blogs

* [companies.json](https://github.com/tdziurko/jvm-bloggers/blob/master/companies.json) for company blogs

and that's all. Once it is merged, you are on the list!
 
## Roadmap

__Done__

1. Send weekly e-mail about latest blog posts to JUG mailing lists
2. Add technical blogs from Polish companies
3. Add moderation to discard off-topic content
4. Write some admin panel to manage moderation. etc.

__Planned__

* Publish global RSS feed with all blog posts
* Create website with all newsletter (latest and old ones)
* Fetch data about number of comments in articles
* Analyze tweets from developers and highlight those with most RTs, stars and the most active conversations
*  Maybe export the idea to another country? :)
* .... this is a place for __YOUR__ idea :)

## Technical details

Application is written using Java 8, Spring Boot, Liquibase, Akka and JPA, running on PostgreSQL database. Currenly it runs on a Heroku.

## Local development setup

#### Step 1: 

You need a PostgreSQL database (name: `jvm_bloggers`, user/pass: `jvm_bloggers`/`jvm_bloggers`). The easiest and recommended way is to run it as docker container:

	docker run --name jvm-bloggers-db -e POSTGRES_USER=jvm_bloggers -e POSTGRES_PASSWORD=jvm_bloggers -p 5432:5432 -d postgres

#### Step 2:

Execute Gradle `bootRun` task:

    ./gradlew  -Djasypt.encryptor.password=<SECRET_PASSWORD> -Dspring.profiles.active=dev -Ddatabase.host=<JVM_BLOGGERS_DB_HOST_ADDRESS> bootRun

**NOTE**:If the `jvm_bloggers` database is running on _localhost_ then `database.host` property can be omitted (it will be assumed to be _localhost_ by default).


#### Step 3:

Navigate to [http://localhost:8080/admin](http://localhost:8080/admin) and fill login form with any login and `<SECRET_PASSWORD>` (the password provided in the previous step)

**NOTE:** Admin UI is based on [http://startbootstrap.com/template-overviews/sb-admin-2/](http://startbootstrap.com/template-overviews/sb-admin-2/).

#### Step 4:

Your local database is probably empty so you need either wait 10 minutes for Schedulers (`BloggersDataFetchingScheduler` and `BlogPostsFetchingScheduler`) to fetch data or change `@Scheduled` annotation in these classes so they execute earlier (eg. `@Scheduled(fixedDelay = 1000)`))

#### Step 5:

To import the project into IDE first execute `./gradlew eclipse` or `./gradlew idea` (depending on your IDE) to generate project files and import them into IDE.

## Running locally with Docker

    docker run -p 8080:8080 --add-host=jvm_bloggers_db:<JVM_BLOGGERS_DB_HOST_ADDRESS> -e database.host=jvm_bloggers_db -e jasypt.encryptor.password="<jasypt_password>" -e spring.profiles.active="dev" tdziurko/jvm-bloggers:<TAG>
    
where TAG is one from https://hub.docker.com/r/tdziurko/jvm-bloggers/tags/ or any tag of your local image repository.      

## Contributing

Wanna help? Please let me know, you can simply:

* join [Gitter room](https://gitter.im/tdziurko/jvm-bloggers)
* write an e-mail to GMail: jvmbloggers (at) (you know what).com :)
* create an issue or comment on existing one 

**Important:** 

1. Before coding please comment on issue that you will be working on so we do not duplicate efforts :)
2. PR with code clenups are also welcome :)

This is a community driven open source project, so any help is appreciated!

#### Contact

If you need direct contact, you can reach JVM Bloggers project at GMail: jvmbloggers (at) (you know what).(you know what too) :)
