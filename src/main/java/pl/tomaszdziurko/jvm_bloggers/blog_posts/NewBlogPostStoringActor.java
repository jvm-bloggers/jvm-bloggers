package pl.tomaszdziurko.jvm_bloggers.blog_posts;

import akka.actor.AbstractActor;
import akka.actor.Props;
import akka.japi.pf.ReceiveBuilder;

import com.sun.syndication.feed.synd.SyndContent;
import com.sun.syndication.feed.synd.SyndEntry;

import lombok.extern.slf4j.Slf4j;

import org.apache.commons.lang3.StringUtils;

import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPostRepository;
import pl.tomaszdziurko.jvm_bloggers.utils.DateTimeUtilities;

import java.util.Date;

import static com.google.common.base.MoreObjects.firstNonNull;

@Slf4j
public class NewBlogPostStoringActor extends AbstractActor {

    public NewBlogPostStoringActor(BlogPostRepository blogPostRepository) {
        receive(ReceiveBuilder.match(RssEntryWithAuthor.class,
            rssEntry -> {
                BlogPost blogPost = blogPostRepository
                    .findByUrl(rssEntry.getRssEntry().getLink())
                    .orElseGet(() -> createBlogPost(rssEntry));
                updateDescription(blogPost, rssEntry.getRssEntry().getDescription());
                blogPostRepository.save(blogPost);
            }).build()
        );
    }

    public static Props props(BlogPostRepository blogPostRepository) {
        return Props.create(NewBlogPostStoringActor.class, () -> {
                return new NewBlogPostStoringActor(blogPostRepository);
            }
        );
    }

    private BlogPost createBlogPost(RssEntryWithAuthor rssEntry) {
        SyndEntry postInRss = rssEntry.getRssEntry();
        Date dateToStore = firstNonNull(postInRss.getPublishedDate(), postInRss.getUpdatedDate());
        log.info("Creating new post '{}' by {}", postInRss.getTitle(),
            rssEntry.getBlog().getAuthor());
        return BlogPost.builder()
            .title(postInRss.getTitle())
            .url(postInRss.getLink())
            .publishedDate(DateTimeUtilities.toLocalDateTime(dateToStore))
            .approved(rssEntry.getBlog().getDefaultApprovedValue())
            .blog(rssEntry.getBlog())
            .build();
    }

    private void updateDescription(BlogPost blogPost, SyndContent descriptionContent) {
        if (descriptionContent != null) {
            String description = descriptionContent.getValue();
            description = StringUtils.abbreviate(description, BlogPost.MAX_DESCRIPTION_LENGTH);
            blogPost.setDescription(description);
        }
    }
}
