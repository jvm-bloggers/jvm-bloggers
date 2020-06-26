package com.jvm_bloggers.core.newsletter_issues


import com.jvm_bloggers.SpringContextAwareSpecification
import com.jvm_bloggers.entities.blog.BlogType
import com.jvm_bloggers.entities.blog_post.BlogPost
import com.jvm_bloggers.entities.blog_post.BlogPostRepository
import com.jvm_bloggers.entities.newsletter_issue.NewsletterIssueRepository
import com.jvm_bloggers.utils.NowProvider
import com.jvm_bloggers.utils.ZoneTimeProvider
import org.springframework.data.domain.Pageable
import spock.lang.Subject

import static com.jvm_bloggers.ObjectMother.aBlogPost
import static com.jvm_bloggers.entities.blog.BlogType.COMPANY
import static com.jvm_bloggers.entities.blog.BlogType.PERSONAL
import static com.jvm_bloggers.entities.blog.BlogType.PODCAST
import static com.jvm_bloggers.entities.blog.BlogType.PRESENTATION
import static java.util.Collections.EMPTY_LIST

@Subject(InitialDevelopmentIssuePublisher)
class InitialDevelopmentIssuePublisherSpec extends SpringContextAwareSpecification {

    NewNewsletterIssuePublisher newNewsletterIssuePublisher = Mock()
    NewsletterIssueRepository newsletterIssueRepository = Mock()
    BlogPostRepository blogPostRepository = Mock()
    NowProvider nowProvider = new ZoneTimeProvider()

    InitialDevelopmentIssuePublisher initialDevelopmentIssuePublisher =
            new InitialDevelopmentIssuePublisher(nowProvider, newNewsletterIssuePublisher, newsletterIssueRepository, blogPostRepository)

    def "Shouldn't create a dev issue when there aren't enough blog posts"() {
        given:
        newsletterIssueRepository.count() >> 0
        blogPostRepository.findBlogPostsOfType(_ as BlogType, _ as Pageable) >> EMPTY_LIST

        when:
        initialDevelopmentIssuePublisher.publishTestDevelopmentIssue()

        then:
        0 * newNewsletterIssuePublisher.publishNewIssue(_)
    }

    def "Shouldn't create a dev issue when there is an issue published"() {
        given:
        newsletterIssueRepository.count() >> 1

        when:
        initialDevelopmentIssuePublisher.publishTestDevelopmentIssue()

        then:
        0 * newNewsletterIssuePublisher.publishNewIssue(_)
    }

    def "Should create an issue when there are enough posts"() {
        given:
        def companyBlogPosts = prepareBlogPostListOfType(COMPANY, initialDevelopmentIssuePublisher.REQUIRED_NUMBER_OF_POSTS)
        def personalBlogPosts = prepareBlogPostListOfType(PERSONAL, initialDevelopmentIssuePublisher.REQUIRED_NUMBER_OF_POSTS)
        def podcastBlogPosts = prepareBlogPostListOfType(PODCAST, initialDevelopmentIssuePublisher.REQUIRED_NUMBER_OF_POSTS)
        def presentationBlogPosts = prepareBlogPostListOfType(PRESENTATION, initialDevelopmentIssuePublisher.REQUIRED_NUMBER_OF_POSTS)

        newsletterIssueRepository.count() >> 0
        blogPostRepository.findBlogPostsOfType(PERSONAL, _ as Pageable) >> personalBlogPosts
        blogPostRepository.findBlogPostsOfType(COMPANY, _ as Pageable) >> companyBlogPosts
        blogPostRepository.findBlogPostsOfType(PODCAST, _ as Pageable) >> podcastBlogPosts
        blogPostRepository.findBlogPostsOfType(PRESENTATION, _ as Pageable) >> presentationBlogPosts
        blogPostRepository.findUnapprovedPostsByBlogType(COMPANY, _ as Pageable) >> companyBlogPosts
        blogPostRepository.findUnapprovedPostsByBlogType(PODCAST, _ as Pageable) >> podcastBlogPosts
        blogPostRepository.findUnapprovedPostsByBlogType(PRESENTATION, _ as Pageable) >> presentationBlogPosts

        when:
        initialDevelopmentIssuePublisher.publishTestDevelopmentIssue()

        then:
        1 * newNewsletterIssuePublisher.publishNewIssue(_)
    }

    def "Should approve posts"() {
        given:
        def unapprovedCompanyBlogPosts = prepareBlogPostListOfType(COMPANY, initialDevelopmentIssuePublisher.REQUIRED_NUMBER_OF_POSTS)
        def personalBlogPosts = prepareBlogPostListOfType(PERSONAL, initialDevelopmentIssuePublisher.REQUIRED_NUMBER_OF_POSTS)
        def unapprovedPodcastBlogPosts = prepareBlogPostListOfType(PODCAST, initialDevelopmentIssuePublisher.REQUIRED_NUMBER_OF_POSTS)
        def unapprovedPresentationBlogPosts = prepareBlogPostListOfType(PRESENTATION, initialDevelopmentIssuePublisher.REQUIRED_NUMBER_OF_POSTS)

        newsletterIssueRepository.count() >> 0
        blogPostRepository.findBlogPostsOfType(COMPANY, _ as Pageable) >> unapprovedCompanyBlogPosts
        blogPostRepository.findBlogPostsOfType(PERSONAL, _ as Pageable) >> personalBlogPosts
        blogPostRepository.findBlogPostsOfType(PODCAST, _ as Pageable) >> unapprovedPodcastBlogPosts
        blogPostRepository.findBlogPostsOfType(PRESENTATION, _ as Pageable) >> unapprovedPresentationBlogPosts
        blogPostRepository.findUnapprovedPostsByBlogType(COMPANY, _ as Pageable) >> unapprovedCompanyBlogPosts
        blogPostRepository.findUnapprovedPostsByBlogType(PODCAST, _ as Pageable) >> unapprovedPodcastBlogPosts
        blogPostRepository.findUnapprovedPostsByBlogType(PRESENTATION, _ as Pageable) >> unapprovedPresentationBlogPosts

        when:
        initialDevelopmentIssuePublisher.publishTestDevelopmentIssue()

        then:
        for (BlogPost post : unapprovedCompanyBlogPosts)
            assert post.isApproved()
        for (BlogPost post : unapprovedPresentationBlogPosts)
            assert post.isApproved()
        for (BlogPost post : unapprovedPodcastBlogPosts)
            assert post.isApproved()
    }

    def prepareBlogPostListOfType(BlogType type, int listSize) {
        return Collections.nCopies(listSize, aBlogPost(blogType: type));
    }
}