package com.jvm_bloggers.entities.newsletter_issue

import com.jvm_bloggers.entities.blog.Blog
import com.jvm_bloggers.entities.blog_post.BlogPost
import spock.lang.Subject

@Subject(NewsletterIssueRepository)
class NewsletterIssueRepositorySpec extends NewsletterIssueRepositorySpecBase {

    def "Should check existence of issue base on issueNumber"() {
        given:
        Long issueNumber = 1L
        List<Blog> blogs = prepareBlogs()
        List<BlogPost> posts = prepareBlogPosts(blogs.get(0), blogs.get(1))
        NewsletterIssue issue = buildNewsletterIssue(issueNumber, blogs, posts, 'heading', 'varia')

        and:
        newsletterIssueRepository.save(issue)

        when:
        boolean newsletterIssueExist = newsletterIssueRepository.existsByIssueNumber(testIssueNumber)

        then:
        newsletterIssueExist == expectedResult

        where:
        testIssueNumber || expectedResult
        1L              || true
        2L              || false
    }

}