package com.jvm_bloggers.core.social.twitter;

import com.jvm_bloggers.core.blogpost_redirect.LinkGenerator;
import com.jvm_bloggers.entities.blog.Blog;
import com.jvm_bloggers.entities.blog_post.BlogPost;
import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssue;

import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssueRepository;
import io.vavr.collection.List;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;
import org.stringtemplate.v4.ST;

import java.util.Objects;

import static lombok.AccessLevel.PACKAGE;

@Component
@Slf4j
@RequiredArgsConstructor(access = PACKAGE)
class TweetContentGenerator {

    private final LinkGenerator linkGenerator;
    private final NewsletterIssueRepository newsletterIssueRepository;

    public String generateTweetContent(NewsletterIssue issue) {
        final List<String> personals =
            List.ofAll(issue.getBlogPosts())
                .map(BlogPost::getBlog)
                .filter(Blog::isPersonal)
                .map(Blog::getTwitter)
                .filter(Objects::nonNull)
                .distinct()
                .shuffle()
                .take(2)
                .padTo(2, null);

        final String company =
            List.ofAll(issue.getBlogPosts())
                .map(BlogPost::getBlog)
                .filter(Blog::isCompany)
                .map(Blog::getTwitter)
                .filter(Objects::nonNull)
                .shuffle()
                .getOrElse((String) null);

        final String issueLink = linkGenerator.generateIssueLink(issue.getIssueNumber());

        final ST template = new ST(TweetContentTemplates.newIssueMessageTemplate());
        template.add("number", issue.getIssueNumber());
        template.add("link", issueLink);
        template.add("personal1", personals.head());
        template.add("personal2", personals.last());
        template.add("company", company);
        return template.render();
    }

    public NewBlogTweetContents generateTweetContent(Blog newBlog) {
        String author = getNewBlogAuthor(newBlog);
        String issueLink = getMostRecentNewsletterIssueLink();

        List<String> shuffledTemplates = TweetContentTemplates.newBlogMessageTemplates().shuffle();

        ST firstTemplate = new ST(shuffledTemplates.head());
        firstTemplate.add("author", author);
        firstTemplate.add("link", issueLink);

        ST secondTemplate = new ST(shuffledTemplates.last());
        secondTemplate.add("author", author);
        secondTemplate.add("link", issueLink);

        return new NewBlogTweetContents(firstTemplate.render(), secondTemplate.render());
    }

    private String getMostRecentNewsletterIssueLink() {
    return newsletterIssueRepository.findFirstByOrderByPublishedDateDesc()
        .map(NewsletterIssue::getIssueNumber)
        .map(linkGenerator::generateIssueLink)
        .getOrElseThrow(() -> new IllegalStateException("No newsletter issue found"));
  }

    private String getNewBlogAuthor(Blog newBlog) {
        String twitter = newBlog.getTwitter();
        return StringUtils.isNotBlank(twitter) ? twitter : newBlog.getAuthor();
    }
}
