package pl.tomaszdziurko.jvm_bloggers.mailing;

import com.google.common.base.Strings;
import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

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
import pl.tomaszdziurko.jvm_bloggers.utils.UriUtmComponentsBuilder;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static java.util.Collections.emptyList;
import static pl.tomaszdziurko.jvm_bloggers.utils.UriUtmComponentsBuilder.DEFAULT_UTM_CAMPAING;
import static pl.tomaszdziurko.jvm_bloggers.utils.UriUtmComponentsBuilder.DEFAULT_UTM_SOURCE;


@Slf4j
@Component
@NoArgsConstructor
@AllArgsConstructor(onConstructor = @__(@Autowired))
public class BlogSummaryMailGenerator {

    private static final char TEMPLATE_DELIMITER = '$';
    private static final String UTM_MEDIUM = "newsletter";

    private BlogRepository blogRepository;
    private BlogPostRepository blogPostRepository;
    private MetadataRepository metadataRepository;
    private NowProvider nowProvider;

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
        template.add("newPosts", postsToMailItems(newPostsFromPersonalBlogs, issueNumber));
        template.add("newPostsFromCompanies",
            postsToMailItems(newPostsfromCompanies, issueNumber));
        template.add("newlyAddedBlogs",
            blogsToMailItems(blogsAddedSinceLastNewsletter, issueNumber));
        return template.render();
    }

    private List<BlogPostForMailItem> postsToMailItems(List<BlogPost> newPosts, long issueNumber) {
        return newPosts.stream().map(blogPost ->
            BlogPostForMailItem.builder()
                .from(blogPost)
                .withIssueNumber(issueNumber)
                .withUrl(urlWithUtmParameters(blogPost.getUrl(), issueNumber))
                .build()
        ).collect(Collectors.toList());
    }

    private List<Blog> blogsToMailItems(List<Blog> blogs, long issueNumber) {
        return blogs.stream()
            .filter(blog -> !Strings.isNullOrEmpty(blog.getUrl()))
            .map(blog -> {
                blog.setUrl(urlWithUtmParameters(blog.getUrl(), issueNumber));
                return blog;
            }).collect(Collectors.toList());
    }

    private String urlWithUtmParameters(String url, long issueNumber) {
        return UriUtmComponentsBuilder.fromHttpUrl(url)
            .withSource(DEFAULT_UTM_SOURCE)
            .withMedium(UTM_MEDIUM)
            .withCampaign(String.format("%s#%s", DEFAULT_UTM_CAMPAING, issueNumber))
            .build();
    }
}
