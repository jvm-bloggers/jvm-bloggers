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
5. Publish global [RSS](http://jvm-bloggers.com/pl/rss) feed with all blog posts. You can limit and exclude authors using parameters. example: http://jvm-bloggers.com/pl/rss?limit=15&excludedAuthors=author1,author2 
6. Add videos from JUGs and conferences in Poland

__Planned__
* Create website with all newsletter (latest and old ones)
* Fetch data about number of comments in articles
* Analyze tweets from developers and highlight those with most RTs, stars and the most active conversations
*  Maybe export the idea to another country? :)
* .... this is a place for __YOUR__ idea :)

## Technical details

* Application is written using Java 8, Spring Boot, Liquibase, Akka and JPA, running on PostgreSQL database. Currenly it runs on a server hosted by [SoftwareMill](http://SoftwareMill.com).

* To import the project into IDE first execute `./gradlew eclipse` or `./gradlew idea` (depending on your IDE) to generate project files and import them into IDE.

* Admin UI is based on [http://startbootstrap.com/template-overviews/sb-admin-2/](http://startbootstrap.com/template-overviews/sb-admin-2/).

# [Development and contribution guidelelins Wiki](https://github.com/tdziurko/jvm-bloggers/wiki)

#### Contact

If you need direct contact, you can reach JVM Bloggers project via:

* GMail: jvmbloggers (at) (you know what).(you know what too) :)
* [Gitter room](https://gitter.im/tdziurko/jvm-bloggers)

