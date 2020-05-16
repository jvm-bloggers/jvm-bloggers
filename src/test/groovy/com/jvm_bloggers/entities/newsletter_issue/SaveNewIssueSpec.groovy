package com.jvm_bloggers.entities.newsletter_issue

import com.jvm_bloggers.entities.blog.Blog
import com.jvm_bloggers.entities.blog_post.BlogPost
import io.vavr.control.Option
import spock.lang.Subject

@Subject(NewsletterIssueRepository)
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
        Option<NewsletterIssue> persistedIssue =
                newsletterIssueRepository.findByIssueNumber(issue.getIssueNumber())
        persistedIssue.isDefined()
        with(persistedIssue.get()) {
            blogPosts
            issueNumber == issueNumber
            heading == exampleHeading
            varia == exampleVaria
            newBlogs*.id.containsAll(blogs*.id)
            blogPosts*.id.containsAll(posts*.id)
        }
    }

}
