package pl.tomaszdziurko.jvm_bloggers.blog_posts;


import akka.actor.AbstractActor;
import akka.actor.Props;
import akka.japi.pf.ReceiveBuilder;
import com.sun.syndication.feed.synd.SyndEntry;
import lombok.extern.slf4j.Slf4j;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPostRepository;
import pl.tomaszdziurko.jvm_bloggers.utils.NowProvider;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Date;
import java.util.Optional;

@Slf4j
public class NewBlogPostStoringActor extends AbstractActor {

    private BlogPostRepository blogPostRepository;

    public NewBlogPostStoringActor(BlogPostRepository blogPostRepository) {
        this.blogPostRepository = blogPostRepository;

        receive(ReceiveBuilder.match(RssEntryWithAuthor.class, rssEntry -> {
                Optional<BlogPost> existingPost = blogPostRepository.findByUrl(rssEntry.getRssEntry().getLink());
                if (!existingPost.isPresent()) {
                    storeNewBlogPost(rssEntry);
                } else {
                    log.debug("Existing post found, skipping save");
                }
            }
        ).build());
    }

    private void storeNewBlogPost(RssEntryWithAuthor rssEntry) {
        SyndEntry postInRss = rssEntry.getRssEntry();
        Date dateToStore = postInRss.getPublishedDate() != null ? postInRss.getPublishedDate() : postInRss.getUpdatedDate();
        LocalDateTime publishedDate = dateToStore.toInstant().atZone(ZoneId.of(NowProvider.ZONE_NAME)).toLocalDateTime();
        BlogPost newBlogPost = new BlogPost(postInRss.getTitle(), rssEntry.getAuthor(), postInRss.getLink(), publishedDate);
        blogPostRepository.save(newBlogPost);
        log.info("Stored new post '{}' with id {} by {}", newBlogPost.getTitle(), newBlogPost.getId(), rssEntry.getAuthor().getName());
    }

    public static Props props(BlogPostRepository blogPostRepository) {
        return Props.create(NewBlogPostStoringActor.class, () -> {
                return new NewBlogPostStoringActor(blogPostRepository);
            }
        );
    }
}
