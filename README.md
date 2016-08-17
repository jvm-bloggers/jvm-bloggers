# JVM Bloggers

    Never miss any blog post from developers around JVM in Poland :)

[![Build Status](https://travis-ci.org/tdziurko/jvm-bloggers.svg?branch=master)](https://travis-ci.org/tdziurko/jvm-bloggers)  [![Coverage Status](https://coveralls.io/repos/tdziurko/jvm-bloggers/badge.svg?branch=master&service=github)](https://coveralls.io/github/tdziurko/jvm-bloggers?branch=master) [![Version Eye](https://www.versioneye.com/user/projects/5798bd4874848d004b927ac7/badge.svg)](https://www.versioneye.com/user/projects/5798bd4874848d004b927ac7) [![Join the chat at https://gitter.im/tdziurko/jvm-bloggers](https://badges.gitter.im/tdziurko/jvm-bloggers.svg)](https://gitter.im/tdziurko/jvm-bloggers?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) [![Sputnik](https://sputnik.ci/conf/badge)](https://sputnik.ci/app#/builds/tdziurko/jvm-bloggers)

## Overview

The goal of the **jvm-bloggers** project is to collect information about developers and companies blogging about JVM-related technologies in Poland, despite these bloggers being old-school
Java fanboys, fancy Scala lovers or niche Gosu experts.

On Fridays 12am, this application is sending a newsletter with all new blog posts from the past 7 days.

If you want to have your group or personal e-mail added, please reach out to me (address below).

I'm promoting blogging as the best activity to share knowledge, passion and learn from each other. And the best way to do that is by helping bloggers to reach out to a wider audience.

##### Your blog/JUG/conference is missing?

Please create a Pull Request adding it to

* [bloggers.json](src/main/resources/blogs/bloggers.json) for personal blogs

* [companies.json](src/main/resources/blogs/companies.json) for company blogs

* [videos.json](src/main/resources/blogs/videos.json) for videos from JUGs/Meetups/Conferences

and that's all. Once it is merged, you are in the list!

## Roadmap

__Done__

- [x] Send weekly e-mail about latest blog posts to JUG mailing lists
- [x] Add technical blogs from Polish companies
- [x] Add moderation to discard off-topic content
- [x] Write some admin panel to manage moderation. etc.
- [x] Publish global [RSS](http://jvm-bloggers.com/pl/rss) feed with all blog posts. You can limit and exclude authors using parameters. example: http://jvm-bloggers.com/pl/rss?limit=15&excludedAuthors=author1,author2
- [x] Add videos from JUGs and conferences in Poland

__Planned__

- [ ] Create website with all newsletter (latest and old ones)
- [ ] Fetch data about number of comments in articles
- [ ] Analyze tweets from developers and highlight those with most RTs, stars and the most active conversations
- [ ]  Maybe export the idea to another country? :)
- [ ] .... this is a place for __YOUR__ idea :)

## Technical details

* Application is written using Java 8, Spring Boot, Liquibase, Akka and JPA, running on PostgreSQL database. It's currently running on a server hosted by the one and only [SoftwareMill](http://SoftwareMill.com).

* To import the project into your IDE execute `./gradlew eclipse` or `./gradlew idea` first (depending on your IDE) to generate project files and import them into IDE.

* Admin UI is based on [http://startbootstrap.com/template-overviews/sb-admin-2/](http://startbootstrap.com/template-overviews/sb-admin-2/).

# [Development and contribution guidelelins Wiki](https://github.com/tdziurko/jvm-bloggers/wiki)

## Contributors

* [Tomasz Dziurko](http://tomaszdziurko.pl) - project leader
* [Marcin Kłopotek](https://github.com/goostleek)
* [Mateusz Urbański](https://github.com/matek2305)
* [Jakub Spręga](http://cslysy.github.io/)
* [Dawid Pura](https://github.com/puradawid)
* [Jacek Jackowiak](https://github.com/airborn)
* [Maciej Szarliński](https://github.com/mszarlinski)
* [Marcin Zajączkowski](https://solidsoft.wordpress.com/)
* [Jonatan Borkowski](https://github.com/jborkowski)
* [Bartłomiej Piech](https://github.com/delor)
* [Sławomir Leski](https://github.com/sleski)

#### Contact

If you need a direct contact, you can reach out to the JVM Bloggers project via:

* GMail: jvmbloggers (at) (you know that).(and you know that too) :)
* [Gitter room](https://gitter.im/tdziurko/jvm-bloggers)
