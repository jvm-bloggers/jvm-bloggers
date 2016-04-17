package pl.tomaszdziurko.jvm_bloggers.mailing;

import com.rometools.rome.feed.synd.SyndFeed;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.stringtemplate.v4.ST;

import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost;
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPostRepository;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.BlogRepository;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.BlogType;
import pl.tomaszdziurko.jvm_bloggers.metadata.Metadata;
import pl.tomaszdziurko.jvm_bloggers.metadata.MetadataKeys;
import pl.tomaszdziurko.jvm_bloggers.metadata.MetadataRepository;
import pl.tomaszdziurko.jvm_bloggers.utils.NowProvider;
import pl.tomaszdziurko.jvm_bloggers.utils.SyndFeedProducer;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

import static java.util.Collections.emptyList;

@Slf4j
@Component
@NoArgsConstructor
@AllArgsConstructor(onConstructor = @__(@Autowired))
public class BlogSummaryMailGenerator {

    private static final char TEMPLATE_DELIMITER = '$';

    private BlogRepository blogRepository;
    private BlogPostRepository blogPostRepository;
    private MetadataRepository metadataRepository;
    private NowProvider nowProvider;
    private SyndFeedProducer syndFeedFactory;

    public String prepareMailContent(int numberOfDaysBackInThePast, long issueNumber) {
        LocalDateTime publishedDate = nowProvider.now()
            .minusDays(numberOfDaysBackInThePast)
            .withHour(11)
            .withMinute(0)
            .withSecond(0)
            .withNano(0);
        List<Blog> blogsAddedSinceLastNewsletter =
            blogRepository.findByDateAddedAfter(publishedDate);
        List<BlogPost> newApprovedPosts = blogPostRepository
            .findByPublishedDateAfterAndApprovedTrueOrderByPublishedDateAsc(publishedDate);
        if (newApprovedPosts.isEmpty() && blogsAddedSinceLastNewsletter.isEmpty()) {
            log.warn("There are no new posts nor new blogs added for last {} days !!!",
                numberOfDaysBackInThePast);
        }

        Map<BlogType, List<BlogPost>> newBlogPostsByType = newApprovedPosts
            .stream()
            .collect(Collectors.groupingBy(it -> it.getBlog().getBlogType()));

        List<BlogPost> newPostsFromPersonalBlogs =
            newBlogPostsByType.getOrDefault(BlogType.PERSONAL, emptyList());
        List<BlogPost> newPostsfromCompanies =
            newBlogPostsByType.getOrDefault(BlogType.COMPANY, emptyList());

        Metadata mailingTemplate = metadataRepository.findByName(MetadataKeys.MAILING_TEMPLATE);
        String templateContent =  mailingTemplate.getValue();
        ST template = new ST(templateContent, TEMPLATE_DELIMITER, TEMPLATE_DELIMITER);
        template.add("days", numberOfDaysBackInThePast);
        template.add("newPosts", toMailItems(newPostsFromPersonalBlogs, issueNumber));
        template.add("newPostsFromCompanies",
            toMailItems(newPostsfromCompanies, issueNumber));
        template.add("blogsWithHomePage",
            getBlogAndItsHomePage(blogsAddedSinceLastNewsletter));
        return template.render();
    }

    private List<BlogPostForMailItem> toMailItems(List<BlogPost> newPosts, long issueNumber) {
        return newPosts.stream().map(blogPost ->
            BlogPostForMailItem.builder()
                .from(blogPost)
                .withIssueNumber(issueNumber)
                .withDefaultUtmParameters()
                .build()
        ).collect(Collectors.toList());
    }

    private Map<Blog, HomePageUrl> getBlogAndItsHomePage(
        List<Blog> blogsAddedSinceLastNewsletter) {
        return blogsAddedSinceLastNewsletter.stream()
            .map(blog -> Pair.of(blog, new HomePageUrl(getBlogHomePageFromRss(blog.getRss()))))
            .collect(Collectors.toMap(Pair::getLeft, Pair::getRight));
    }

    private Optional<String> getBlogHomePageFromRss(String rss) {
        return syndFeedFactory.urlFromRss(rss);
    }

    public static class HomePageUrl {

        @Getter
        private final String url;

        @Getter
        private final boolean isAvailable;

        private HomePageUrl(Optional<String> optUrlString) {
            isAvailable = optUrlString.isPresent();
            url = optUrlString.orElse(StringUtils.EMPTY);
        }

    }

}
