package com.jvm_bloggers.core.mailing;

import com.google.common.base.Strings;
import com.jvm_bloggers.core.blogpost_redirect.LinkGenerator;
import com.jvm_bloggers.core.utils.UriUtmComponentsBuilder;
import com.jvm_bloggers.entities.blog.Blog;
import com.jvm_bloggers.entities.blog.BlogType;
import com.jvm_bloggers.entities.blog_post.BlogPost;
import com.jvm_bloggers.entities.metadata.MetadataKeys;
import com.jvm_bloggers.entities.metadata.MetadataRepository;
import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssue;

import io.vavr.collection.List;
import io.vavr.collection.Map;

import lombok.NoArgsConstructor;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.stringtemplate.v4.ST;

import java.util.Comparator;

import static com.jvm_bloggers.core.utils.UriUtmComponentsBuilder.DEFAULT_UTM_CAMPAING;
import static com.jvm_bloggers.core.utils.UriUtmComponentsBuilder.DEFAULT_UTM_SOURCE;
import static com.jvm_bloggers.utils.HtmlEmptinessChecker.isEmpty;
import static io.vavr.collection.List.empty;

@Component
@NoArgsConstructor
public class BlogSummaryMailGenerator {

    private static final int DAYS_IN_THE_PAST = 7;
    private static final char TEMPLATE_DELIMITER = '$';
    private static final String UTM_MEDIUM = "newsletter";

    private static final String NEW_LINE = "<br/>";

    private MetadataRepository metadataRepository;
    private LinkGenerator linkGenerator;

    @Autowired
    public BlogSummaryMailGenerator(
        MetadataRepository metadataRepository,
        LinkGenerator linkGenerator
    ) {
        this.metadataRepository = metadataRepository;
        this.linkGenerator = linkGenerator;
    }

    public String prepareMailContent(NewsletterIssue newsletterIssue) {
        String greeting = prepareGreetingSection(newsletterIssue.getIssueNumber());
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

    private String prepareGreetingSection(Long issueNumber) {
        String templateContent = getValueForSection(MetadataKeys.MAILING_GREETING);
        ST template = new ST(templateContent, TEMPLATE_DELIMITER, TEMPLATE_DELIMITER);
        template.add("currentIssueLink", linkGenerator.generateIssueLink(issueNumber));
        return template.render();
    }

    private String getValueForSection(String key) {
        return metadataRepository.findByName(key).getValue();
    }

    private String appendNewLinesIfNotEmpty(String text, int numberOfNewLines) {
        if (isEmpty(text)) {
            return "";
        } else {
            return text + StringUtils.repeat(NEW_LINE, numberOfNewLines);
        }
    }

    private String prepareVariaSection(String variaContent, int numberOfNewLines) {
        if (isEmpty(variaContent)) {
            return "";
        } else {
            return "Varia:" + NEW_LINE + variaContent
                + StringUtils.repeat(NEW_LINE, numberOfNewLines);
        }
    }

    private String prepareMainSectionWithBlogs(NewsletterIssue newsletterIssue) {
        List<Blog> blogsAddedSinceLastNewsletter = List.ofAll(newsletterIssue.getNewBlogs());
        List<BlogPost> newApprovedPosts = List.ofAll(newsletterIssue.getBlogPosts());

        Map<BlogType, List<BlogPost>> newBlogPostsByType = newApprovedPosts
            .groupBy(it -> it.getBlog().getBlogType());

        List<BlogPost> newPersonalPosts =
            newBlogPostsByType.getOrElse(BlogType.PERSONAL, empty());
        List<BlogPost> newCompanyPosts =
            newBlogPostsByType.getOrElse(BlogType.COMPANY, empty());
        List<BlogPost> newPresentations =
            newBlogPostsByType.getOrElse(BlogType.PRESENTATION, empty());
        List<BlogPost> newPodcasts =
                newBlogPostsByType.getOrElse(BlogType.PODCAST, empty());

        String templateContent = getValueForSection(MetadataKeys.MAILING_TEMPLATE);
        ST template = new ST(templateContent, TEMPLATE_DELIMITER, TEMPLATE_DELIMITER);
        template.add("days", DAYS_IN_THE_PAST);
        template.add("newPersonalPosts",
            postsToMailItems(newPersonalPosts, newsletterIssue.getIssueNumber())
            .toJavaArray()
        );
        template.add("newCompanyPosts",
            postsToMailItems(newCompanyPosts, newsletterIssue.getIssueNumber()).toJavaList()
        );
        template.add(
                "newPresentationPosts",
                postsToMailItems(newPresentations, newsletterIssue.getIssueNumber()).toJavaList()
        );
        template.add(
                "newPodcastPosts",
                postsToMailItems(newPodcasts, newsletterIssue.getIssueNumber()).toJavaList()
        );

        template.add("newlyAddedBlogs",
            blogsToMailItems(blogsAddedSinceLastNewsletter, newsletterIssue.getIssueNumber())
                .toJavaList()
        );

        return template.render();
    }

    private List<BlogPostForMailItem> postsToMailItems(List<BlogPost> newPosts, long issueNumber) {
        return newPosts
            .sorted(Comparator.comparing(b -> b.getBlog().getAuthor()))
            .map(blogPost ->
                BlogPostForMailItem.builder()
                    .from(blogPost)
                    .withIssueNumber(issueNumber)
                    .withUrl(linkGenerator.generateRedirectLinkFor(blogPost.getUid()))
                    .build()
            );
    }

    private List<Blog> blogsToMailItems(List<Blog> blogs, long issueNumber) {
        return blogs
            .filter(blog -> !Strings.isNullOrEmpty(blog.getUrl()))
            .map(blog -> {
                blog.setUrl(urlWithUtmParameters(blog.getUrl(), issueNumber));
                return blog;
            });
    }

    private String urlWithUtmParameters(String url, long issueNumber) {
        return UriUtmComponentsBuilder.fromHttpUrl(url)
            .withSource(DEFAULT_UTM_SOURCE)
            .withMedium(UTM_MEDIUM)
            .withCampaign(String.format("%s#%s", DEFAULT_UTM_CAMPAING, issueNumber))
            .build();
    }

}
