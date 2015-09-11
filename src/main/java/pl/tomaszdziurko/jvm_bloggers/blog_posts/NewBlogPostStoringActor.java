package pl.tomaszdziurko.jvm_bloggers.blog_posts;


import akka.actor.AbstractActor;
import akka.actor.Props;
import akka.japi.pf.ReceiveBuilder;
import com.sun.syndication.feed.synd.SyndEntry;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class NewBlogPostStoringActor extends AbstractActor {

    public NewBlogPostStoringActor() {
        receive(ReceiveBuilder.match(SyndEntry.class, blogPost -> {
                log.info("blog post " + blogPost.getLink());

            }
        ).build());
    }


    public static Props props() {
        return Props.create(NewBlogPostStoringActor.class);
    }
}
