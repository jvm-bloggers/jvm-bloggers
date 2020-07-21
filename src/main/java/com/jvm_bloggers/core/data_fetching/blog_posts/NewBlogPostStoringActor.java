package com.jvm_bloggers.core.data_fetching.blog_posts;

import akka.actor.AbstractActor;
import akka.actor.Props;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@RequiredArgsConstructor
public class NewBlogPostStoringActor extends AbstractActor {

    private final BlogPostService blogPostService;

    public static Props props(BlogPostService blogPostService) {
        return Props.create(
                NewBlogPostStoringActor.class,
                () -> new NewBlogPostStoringActor(blogPostService)
        );
    }

    @Override
    public Receive createReceive() {
        return receiveBuilder().match(RssEntryWithAuthor.class,
                blogPostService::addOrUpdate).build();
    }

}
