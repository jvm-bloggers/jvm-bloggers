package pl.tomaszdziurko.jvm_bloggers.mailing;

import com.google.common.base.Strings;

import lombok.NoArgsConstructor;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.stringtemplate.v4.ST;

import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.BlogType;
import pl.tomaszdziurko.jvm_bloggers.click_counter.RedirectLinkGenerator;
import pl.tomaszdziurko.jvm_bloggers.metadata.MetadataKeys;
import pl.tomaszdziurko.jvm_bloggers.metadata.MetadataRepository;
import pl.tomaszdziurko.jvm_bloggers.newsletter_issues.domain.NewsletterIssue;
import pl.tomaszdziurko.jvm_bloggers.utils.UriUtmComponentsBuilder;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static java.util.Collections.emptyList;
import static pl.tomaszdziurko.jvm_bloggers.utils.UriUtmComponentsBuilder.DEFAULT_UTM_CAMPAING;
import static pl.tomaszdziurko.jvm_bloggers.utils.UriUtmComponentsBuilder.DEFAULT_UTM_SOURCE;


@Component
@NoArgsConstructor
public class BlogSummaryMailGenerator {

    private static final int DAYS_IN_THE_PAST = 7;
    private static final char TEMPLATE_DELIMITER = '$';
    private static final String UTM_MEDIUM = "newsletter";

    private static final String NEW_LINE = "<br/>";

    private MetadataRepository metadataRepository;
    private RedirectLinkGenerator linkGenerator;

    @Autowired
    public BlogSummaryMailGenerator(
        MetadataRepository metadataRepository,
        RedirectLinkGenerator linkGenerator
    ) {
        this.metadataRepository = metadataRepository;
        this.linkGenerator = linkGenerator;
    }

    public String prepareMailContent(NewsletterIssue newsletterIssue) {
        String greeting = getValueForSection(MetadataKeys.MAILING_GREETING);
        String heading = newsletterIssue.getHeading();
        String mainSection = prepareMainSectionWithBlogs(newsletterIssue);
        String varia = newsletterIssue.getVaria();
        String signature = getValueForSection(MetadataKeys.MAILING_SIGNATURE);

        return appendNewLinesIfNotEmpty(greeting, 2)
            + appendNewLinesIfNotEmpty(heading, 2)
            + appendNewLinesIfNotEmpty(mainSection, 2)
            + prepareVariaSection(varia, 2)
            + NEW_LINE + signature;
    }

    private String getValueForSection(String key) {
        return metadataRepository.findByName(key).getValue();
    }

    private String appendNewLinesIfNotEmpty(String text, int numberOfNewLines) {
        if (StringUtils.isEmpty(text)) {
            return text;
        } else {
            return text + StringUtils.repeat(NEW_LINE, numberOfNewLines);
        }
    }

    private String prepareVariaSection(String variaContent, int numberOfNewLines) {
        if (StringUtils.isEmpty(variaContent)) {
            return variaContent;
        } else {
            return "Varia:" + NEW_LINE + variaContent
                + StringUtils.repeat(NEW_LINE, numberOfNewLines);
        }
    }

    private String prepareMainSectionWithBlogs(NewsletterIssue newsletterIssue) {
        List<Blog> blogsAddedSinceLastNewsletter = newsletterIssue.getNewBlogs();
        List<BlogPost> newApprovedPosts = newsletterIssue.getBlogPosts();

        Map<BlogType, List<BlogPost>> newBlogPostsByType = newApprovedPosts
            .stream()
            .collect(Collectors.groupingBy(it -> it.getBlog().getBlogType()));

        List<BlogPost> newPostsFromPersonalBlogs =
            newBlogPostsByType.getOrDefault(BlogType.PERSONAL, emptyList());
        List<BlogPost> newPostsFromCompanies =
            newBlogPostsByType.getOrDefault(BlogType.COMPANY, emptyList());
        List<BlogPost> newVideoPosts =
            newBlogPostsByType.getOrDefault(BlogType.VIDEOS, emptyList());

        String templateContent = getValueForSection(MetadataKeys.MAILING_TEMPLATE);
        ST template = new ST(templateContent, TEMPLATE_DELIMITER, TEMPLATE_DELIMITER);
        template.add("days", DAYS_IN_THE_PAST);
        template.add("newPosts",
            postsToMailItems(newPostsFromPersonalBlogs, newsletterIssue.getIssueNumber()));
        template.add("newPostsFromCompanies",
            postsToMailItems(newPostsFromCompanies, newsletterIssue.getIssueNumber()));
        template.add("newlyAddedBlogs",
            blogsToMailItems(blogsAddedSinceLastNewsletter, newsletterIssue.getIssueNumber()));
        template.add("newVideoPosts",
            postsToMailItems(newVideoPosts, newsletterIssue.getIssueNumber()));
        return template.render();
    }

    private List<BlogPostForMailItem> postsToMailItems(List<BlogPost> newPosts, long issueNumber) {
        return newPosts.stream().map(blogPost ->
            BlogPostForMailItem.builder()
                .from(blogPost)
                .withIssueNumber(issueNumber)
                .withUrl(linkGenerator.generateLinkFor(blogPost.getUid()))
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
