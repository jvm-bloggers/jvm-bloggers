package com.jvm_bloggers.core.data_fetching.blog_posts

import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import akka.testkit.JavaTestKit
import com.rometools.rome.feed.synd.SyndContent
import com.rometools.rome.feed.synd.SyndEntry
import com.jvm_bloggers.core.data_fetching.blog_posts.domain.BlogPost
import com.jvm_bloggers.core.data_fetching.blog_posts.domain.BlogPostRepository
import com.jvm_bloggers.core.data_fetching.blogs.domain.Blog
import com.jvm_bloggers.utils.DateTimeUtilities
import scala.concurrent.duration.FiniteDuration
import spock.lang.Specification
import spock.lang.Subject

class NewBlogPostStoringActorSpec extends Specification {

    BlogPostRepository blogPostRepository
    JavaTestKit testProbe

    @Subject
    ActorRef blogPostingActor

    def setup() {
        ActorSystem system = ActorSystem.create("test")
        testProbe = new JavaTestKit(system)
        blogPostRepository = Mock(BlogPostRepository)
        Props props = NewBlogPostStoringActor.props(blogPostRepository)
        blogPostingActor = system.actorOf(props, "blogPostingActor")
    }

    def cleanup() {
        testProbe.system.shutdown()
    }

    def "Should persist new blog post"() {
        given:
            String postUrl = "http://blogpost.com/blog"
            String postTitle = "Title"
            String postDescription = "description"
            SyndEntry entry = mockSyndEntry(postUrl, postTitle, postDescription)
            RssEntryWithAuthor message = new RssEntryWithAuthor(Mock(Blog), entry)
            blogPostRepository.findByUrl(postUrl) >> Optional.empty()
        when:
            blogPostingActor.tell(message, ActorRef.noSender())
            testProbe.expectNoMsg(FiniteDuration.apply(1, "second"))
        then:
            1 * blogPostRepository.save({
                    it.url == postUrl &&
                    it.title == postTitle &&
                    it.description == postDescription
            })
    }

    def "Should not persist blog post with invalid URL"() {
        given:
            String invalidLink = "invalidLink"
            String postTitle = "Title"
            String postDescription = "description"
            SyndEntry entry = mockSyndEntry(invalidLink, postTitle, postDescription)
            RssEntryWithAuthor message = new RssEntryWithAuthor(Mock(Blog), entry)
            blogPostRepository.findByUrl(invalidLink) >> Optional.empty()
        when:
            blogPostingActor.tell(message, ActorRef.noSender())
            testProbe.expectNoMsg(FiniteDuration.apply(1, "second"))
        then:
            0 * blogPostRepository.save({
                it.url == invalidLink &&
                        it.title == postTitle &&
                        it.description == postDescription
            })
    }

    def "Should update description if post already exists"() {
        given:
            String postUrl = "http://blogpost.com/blog"
            String postTitle = "Title"
            String postDescription = "description"
            SyndEntry entry = mockSyndEntry(postUrl, postTitle, postDescription)
            BlogPost blogPost = Mock()
            RssEntryWithAuthor message = new RssEntryWithAuthor(Mock(Blog), entry)
            blogPostRepository.findByUrl(postUrl) >> Optional.of(blogPost)
        when:
            blogPostingActor.tell(message, ActorRef.noSender())
            testProbe.expectNoMsg(FiniteDuration.apply(1, "second"))
        then:
            1 * blogPost.setDescription(postDescription)
        then:
            1 * blogPostRepository.save(blogPost)
    }

    def "Should use updatedDate if publishedDate is null"() {
        given:
            String postUrl = "http://blogpost.com/blog"
            String postTitle = "Title"
            Date updatedDate = new Date().minus(1)
            SyndEntry entry = mockSyndEntry(postUrl, postTitle, null, null, updatedDate)
            RssEntryWithAuthor message = new RssEntryWithAuthor(Mock(Blog), entry)
            blogPostRepository.findByUrl(postUrl) >> Optional.empty()
        when:
            blogPostingActor.tell(message, ActorRef.noSender())
            testProbe.expectNoMsg(FiniteDuration.apply(1, "second"))
        then:
            1 * blogPostRepository.save({
                    it.url == postUrl &&
                    it.title == postTitle &&
                    it.publishedDate == DateTimeUtilities.toLocalDateTime(updatedDate)
            })
    }


    private SyndEntry mockSyndEntry(String postUrl, String postTitle, String postDescription) {
        return mockSyndEntry(postUrl, postTitle, postDescription, new Date(), new Date())
    }

    private SyndEntry mockSyndEntry(String postUrl, String postTitle, String postDescription, Date publishedDate, Date updatedDate) {
        SyndEntry entry = Mock(SyndEntry)
        entry.getPublishedDate() >> publishedDate
        entry.getUpdatedDate() >> updatedDate
        entry.getLink() >> postUrl
        entry.getTitle() >> postTitle
        entry.getDescription() >> Stub(SyndContent) {
            getValue() >> postDescription
        }
        return entry
    }

}
