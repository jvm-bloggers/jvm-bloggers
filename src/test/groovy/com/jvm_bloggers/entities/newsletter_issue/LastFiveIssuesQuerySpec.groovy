package com.jvm_bloggers.entities.newsletter_issue

import com.jvm_bloggers.core.newsletter_issues.domain.NewsletterIssueBaseData
import com.jvm_bloggers.entities.blog.Blog
import com.jvm_bloggers.entities.blog_post.BlogPost

import java.time.LocalDate
import java.util.stream.Collectors

class LastFiveIssuesQuerySpec extends NewsletterIssueRepositorySpecBase {

    def "Should find last 5 newsletter issues"() {
        given:
            List<NewsletterIssueBaseData> lastTenIssues = populateTenNewsletterIssues()
        when:
            List<NewsletterIssueBaseData> lastFiveIssues = newsletterIssueRepository.findTop5ByOrderByPublishedDateDesc()
        then:
            lastFiveIssues.size == 5
            lastFiveIssues.containsAll(lastTenIssues.stream().skip(5).collect(Collectors.toList()))
    }

    def "Should have the newest issue on top"() {
        given:
            List<NewsletterIssueBaseData> lastTenIssues = populateTenNewsletterIssues()
        when:
            List<NewsletterIssueBaseData> lastFiveIssues = newsletterIssueRepository.findTop5ByOrderByPublishedDateDesc()
        then:
            lastFiveIssues[0] == lastTenIssues[9]
            lastFiveIssues[1] == lastTenIssues[8]
            lastFiveIssues[2] == lastTenIssues[7]
            lastFiveIssues[3] == lastTenIssues[6]
            lastFiveIssues[4] == lastTenIssues[5]
    }
    
    private List<NewsletterIssueBaseData> populateTenNewsletterIssues(){

        List<Blog> blogs = prepareBlogs()
        List<BlogPost> posts = prepareBlogPosts(blogs.get(0), blogs.get(1))

        List<NewsletterIssueBaseData> persistedIssues = new ArrayList<>()
        for (issueNumber in 1L..10L ) {
            LocalDate publishedDate = LocalDate.of(2010, 5, 5).plusDays(issueNumber * 7)
            persistedIssues.add(newsletterIssueRepository.save(prepareNewsletterIssue(issueNumber, blogs, posts, "Example heading", "Example varia").publishedDate(publishedDate).build()))
        }
        return persistedIssues
    }
}
