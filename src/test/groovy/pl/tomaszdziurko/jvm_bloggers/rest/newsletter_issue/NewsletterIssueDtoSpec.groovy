package pl.tomaszdziurko.jvm_bloggers.rest.newsletter_issue

import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.BlogType
import pl.tomaszdziurko.jvm_bloggers.newsletter_issues.domain.NewsletterIssue
import spock.lang.Specification

import java.time.LocalDate

import static java.time.LocalDateTime.now

class NewsletterIssueDtoSpec extends Specification {

    def "Should convert newsletter issue to its DTO representation"() {
        given:
            Blog blog = new Blog(1, 2, "some author", "some rss", "some url",
                    "some twitter", now(), BlogType.PERSONAL, true)
            BlogPost post = new BlogPost("uid", 1, "some title", "some description", "some url", now(), true, blog)
            NewsletterIssue issue = new NewsletterIssue(1, 2, LocalDate.now(), "Some heading", [post],
                    [blog], "Some varia")
        when:
            NewsletterIssueDto issueDto = NewsletterIssueDto.fromIssue(issue)
        then:
            issueDto.heading == issue.getHeading()
            issueDto.varia == issue.getVaria()
            issueDto.number == issue.getIssueNumber()
            issueDto.publishedDate == issue.getPublishedDate()
            issueDto.newBlogs.first().author == blog.getAuthor()
            issueDto.posts.first().title == post.getTitle()
    }

}
