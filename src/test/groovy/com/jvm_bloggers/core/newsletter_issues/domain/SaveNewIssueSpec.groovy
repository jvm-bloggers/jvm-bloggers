package com.jvm_bloggers.core.newsletter_issues.domain

import com.jvm_bloggers.core.data_fetching.blog_posts.domain.BlogPost
import com.jvm_bloggers.core.data_fetching.blogs.domain.Blog

class SaveNewIssueSpec extends NewsletterIssueRepositorySpecBase {

    def "Should save new issue"() {
        given:
            String exampleHeading = "Example heading"
            String exampleVaria = "Example varia"
            long issueNumber = 77L
        and:
            List<Blog> blogs = prepareBlogs()
            List<BlogPost> posts = prepareBlogPosts(blogs.get(0), blogs.get(1))
            NewsletterIssue issue = buildNewsletterIssue(issueNumber, blogs, posts, exampleHeading, exampleVaria)
        when:
            newsletterIssueRepository.save(issue)
        then:
            Optional<NewsletterIssue> persistedIssue =
                    newsletterIssueRepository.findByIssueNumber(issue.getIssueNumber())
            persistedIssue.isPresent()
            persistedIssue.get().blogPosts
            persistedIssue.get().issueNumber == issueNumber
            persistedIssue.get().heading == exampleHeading
            persistedIssue.get().varia == exampleVaria
            persistedIssue.get().newBlogs*.id.containsAll(blogs*.id)
            persistedIssue.get().blogPosts*.id.containsAll(posts*.id)
    }
}