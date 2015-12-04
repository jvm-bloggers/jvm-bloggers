# JVM Bloggers
    Never miss any blog post from developers around JVM in Poland :)

[![Build Status](https://travis-ci.org/tdziurko/jvm-bloggers.svg?branch=master)](https://travis-ci.org/tdziurko/jvm-bloggers)  [![Coverage Status](https://coveralls.io/repos/tdziurko/jvm-bloggers/badge.svg?branch=master&service=github)](https://coveralls.io/github/tdziurko/jvm-bloggers?branch=master)

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

__Planned__

1. Write some admin panel to manage moderation. etc.
2. Send information about new blog posts using Twitter and Facebook profiles
3. Count clicks to find most popular articles and promote them
4. Fetch data about number of comments in articles
5. Analyze tweets from developers and highlight those with most RTs, stars and the most active conversations
6. Maybe export the idea another country? :)
7. .... this is a place for __YOUR__ idea :)

## Technical details

Application is written using Java 8, Spring Boot, Liquibase, Akka and JPA, running on PostgreSQL database. Currenly it runs on a Heroku.

## Contributing

Wanna help? Please let me know (create an issue, comment on existing one so we don't duplicate work). This is a community driven open source project, so any help is appreciated!

#### Contact

If you need direct contact, you can reach JVM Bloggers project at GMail: jvmbloggers (at) (you know what).(you know what too) :)
