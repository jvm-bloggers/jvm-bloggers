package com.jvm_bloggers.core.newsletter_issues

import com.google.common.collect.Lists
import com.jvm_bloggers.SpringContextAwareSpecification
import com.jvm_bloggers.entities.blog.Blog
import com.jvm_bloggers.entities.blog.BlogType
import com.jvm_bloggers.entities.blog_post.BlogPost
import com.jvm_bloggers.entities.blog_post.BlogPostRepository
import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssueRepository
import com.jvm_bloggers.utils.NowProvider
import com.jvm_bloggers.utils.ZoneTimeProvider
import org.springframework.data.domain.PageRequest
import spock.lang.Subject
import static com.jvm_bloggers.ObjectMother.aBlog
import static com.jvm_bloggers.ObjectMother.aBlogPost

@Subject(InitialDevelopmentIssuePublisher)
class InitialDevelopmentIssuePublisherSpec extends SpringContextAwareSpecification {

    NewNewsletterIssuePublisher newNewsletterIssuePublisher = Mock()
    NewsletterIssueRepository newsletterIssueRepository = Mock()
    BlogPostRepository blogPostRepository = Mock()
    NowProvider nowProvider = new ZoneTimeProvider()

    InitialDevelopmentIssuePublisher initialDevelopmentIssuePublisher =
            new InitialDevelopmentIssuePublisher(nowProvider, newNewsletterIssuePublisher, newsletterIssueRepository, blogPostRepository)
    private int REQUIRED_AMOUNT_OF_POSTS = initialDevelopmentIssuePublisher.REQUIRED_AMOUNT_OF_POSTS
    private int AMOUNT_OF_POSTS_TO_APPROVE = initialDevelopmentIssuePublisher.AMOUNT_OF_POSTS_TO_APPROVE
    private PageRequest requiredSizePageRequest = PageRequest.of(0, REQUIRED_AMOUNT_OF_POSTS)
    private PageRequest amountOfPostsToApprovePageRequest = PageRequest.of(0, AMOUNT_OF_POSTS_TO_APPROVE)

    def "Shouldn't post a dev issue when there aren't enough blog posts"() {
        given:
        newsletterIssueRepository.count() >> 0
        blogPostRepository.findBlogPostsOfType(BlogType.COMPANY, requiredSizePageRequest) >> Lists.newArrayList()
        blogPostRepository.findBlogPostsOfType(BlogType.VIDEOS, requiredSizePageRequest) >> Lists.newArrayList()
        blogPostRepository.findBlogPostsOfType(BlogType.PERSONAL, requiredSizePageRequest) >> Lists.newArrayList()

        when:
        initialDevelopmentIssuePublisher.publishTestDevelopmentIssue()

        then:
        0 * newNewsletterIssuePublisher.publishNewIssue(_)
    }

    def "Shouldn't post a dev issue when there is an issue published"() {
        given:
        newsletterIssueRepository.count() >> 1

        when:
        initialDevelopmentIssuePublisher.publishTestDevelopmentIssue()

        then:
        0 * newNewsletterIssuePublisher.publishNewIssue(_)
    }

    def "Should post an issue when there are enough posts"() {
        Blog companyBlog = aBlog(blogType: BlogType.COMPANY)
        Blog videoBlog = aBlog(blogType: BlogType.VIDEOS)
        Blog personalBlog = aBlog(blogType: BlogType.PERSONAL)

        List<BlogPost> companyBlogPosts = new ArrayList<>()
        List<BlogPost> personalBlogPosts = new ArrayList<>()
        List<BlogPost> videoBlogPosts = new ArrayList<>()
        for (int i = 0; i < REQUIRED_AMOUNT_OF_POSTS; i++) {
            companyBlogPosts.add(aBlogPost(blog: companyBlog))
            personalBlogPosts.add(aBlogPost(blog: personalBlog))
            videoBlogPosts.add(aBlogPost(blog: videoBlog))
        }

        given:
        newsletterIssueRepository.count() >> 0
        blogPostRepository.findBlogPostsOfType(BlogType.PERSONAL, requiredSizePageRequest) >> personalBlogPosts
        blogPostRepository.findBlogPostsOfType(BlogType.COMPANY, requiredSizePageRequest) >> companyBlogPosts
        blogPostRepository.findBlogPostsOfType(BlogType.VIDEOS, requiredSizePageRequest) >> videoBlogPosts
        blogPostRepository.findUnapprovedPostsByBlogType(BlogType.COMPANY, amountOfPostsToApprovePageRequest) >> companyBlogPosts
        blogPostRepository.findUnapprovedPostsByBlogType(BlogType.VIDEOS, amountOfPostsToApprovePageRequest) >> videoBlogPosts

        when:
        initialDevelopmentIssuePublisher.publishTestDevelopmentIssue()

        then:
        1 * newNewsletterIssuePublisher.publishNewIssue(_)
    }

    def "Should approve posts"() {
        Blog companyBlog = aBlog(blogType: BlogType.COMPANY)
        Blog videoBlog = aBlog(blogType: BlogType.VIDEOS)
        Blog personalBlog = aBlog(blogType: BlogType.PERSONAL)

        List<BlogPost> unapprovedCompanyBlogPosts = new ArrayList<>()
        List<BlogPost> personalBlogPosts = new ArrayList<>()
        List<BlogPost> unapprovedVideoBlogPosts = new ArrayList<>()
        for (int i = 0; i < REQUIRED_AMOUNT_OF_POSTS; i++) {
            unapprovedCompanyBlogPosts.add(aBlogPost(blog: companyBlog))
            personalBlogPosts.add(aBlogPost(blog: personalBlog))
            unapprovedVideoBlogPosts.add(aBlogPost(blog: videoBlog))
        }

        given:
        newsletterIssueRepository.count() >> 0
        blogPostRepository.findBlogPostsOfType(BlogType.COMPANY, requiredSizePageRequest) >> unapprovedCompanyBlogPosts
        blogPostRepository.findBlogPostsOfType(BlogType.PERSONAL, requiredSizePageRequest) >> personalBlogPosts
        blogPostRepository.findBlogPostsOfType(BlogType.VIDEOS, requiredSizePageRequest) >> unapprovedVideoBlogPosts
        blogPostRepository.findUnapprovedPostsByBlogType(BlogType.COMPANY, amountOfPostsToApprovePageRequest) >> unapprovedCompanyBlogPosts
        blogPostRepository.findUnapprovedPostsByBlogType(BlogType.VIDEOS, amountOfPostsToApprovePageRequest) >> unapprovedVideoBlogPosts

        when:
        initialDevelopmentIssuePublisher.publishTestDevelopmentIssue()

        then:
        for (BlogPost post : unapprovedCompanyBlogPosts) post.getApprovalState().equalsIgnoreCase("Approved")
        for (BlogPost post : unapprovedVideoBlogPosts) post.getApprovalState().equalsIgnoreCase("Approved")
    }
}