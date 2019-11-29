package com.jvm_bloggers.core.data_fetching.blog_posts

import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import akka.testkit.JavaTestKit
import com.jvm_bloggers.entities.blog.Blog
import com.jvm_bloggers.entities.blog_post.BlogPost
import com.jvm_bloggers.entities.blog_post.BlogPostRepository
import com.rometools.rome.feed.synd.SyndContent
import com.rometools.rome.feed.synd.SyndEntry
import io.vavr.control.Option
import scala.concurrent.duration.FiniteDuration
import spock.lang.Specification
import spock.lang.Subject

import static com.jvm_bloggers.utils.DateTimeUtilities.toLocalDateTime

@Subject(ActorRef)
class NewBlogPostStoringActorSpec extends Specification {

    BlogPostRepository blogPostRepository
    BlogPostFactory blogPostFactory
    JavaTestKit testProbe
    ActorRef blogPostingActor

    String postTitle = 'Title'
    String postDescription = 'description'

    def setup() {
        ActorSystem system = ActorSystem.create('test')
        testProbe = new JavaTestKit(system)
        blogPostRepository = Mock(BlogPostRepository)
        blogPostFactory = Mock(BlogPostFactory)
        Props props = NewBlogPostStoringActor.props(blogPostRepository, blogPostFactory)
        blogPostingActor = system.actorOf(props, 'blogPostingActor')
    }

    def cleanup() {
        testProbe.system.terminate()
    }

    def "Should persist new blog post"() {
        given:
        String postUrl = 'http://blogpost.com/blog'
        Blog blog = Mock(Blog)
        BlogPost blogPost = Mock(BlogPost)
        SyndEntry entry = mockSyndEntry(postUrl, postTitle, postDescription)
        RssEntryWithAuthor message = new RssEntryWithAuthor(blog, entry)
        blogPostFactory.create(postTitle, postUrl, toLocalDateTime(entry.getPublishedDate()), blog) >> blogPost
        blogPostRepository.findByUrlEndingWith(removeHttpProtocolFrom(postUrl)) >> Option.none()

        when:
        blogPostingActor.tell(message, ActorRef.noSender())
        testProbe.expectNoMsg(FiniteDuration.apply(1, 'second'))

        then:
        1 * blogPostRepository.save(blogPost)
    }

    def "Should not persist blog post with invalid URL"() {
        given:
        String invalidLink = 'invalidLink'
        SyndEntry entry = mockSyndEntry(invalidLink, postTitle, postDescription)
        RssEntryWithAuthor message = new RssEntryWithAuthor(Mock(Blog), entry)
        blogPostRepository.findByUrlEndingWith(invalidLink) >> Option.none()

        when:
        blogPostingActor.tell(message, ActorRef.noSender())
        testProbe.expectNoMsg(FiniteDuration.apply(1, 'second'))

        then:
        0 * blogPostRepository.save({
            it.url == invalidLink &&
                it.title == postTitle &&
                it.description == postDescription
        })
    }

    def "Should update description if post already exists"() {
        given:
        String postUrl = 'http://blogpost.com/blog'
        SyndEntry entry = mockSyndEntry(postUrl, postTitle, postDescription)
        BlogPost blogPost = Mock()
        RssEntryWithAuthor message = new RssEntryWithAuthor(Mock(Blog), entry)
        blogPostRepository.findByUrlEndingWith(removeHttpProtocolFrom(postUrl)) >> Option.of(blogPost)

        when:
        blogPostingActor.tell(message, ActorRef.noSender())
        testProbe.expectNoMsg(FiniteDuration.apply(1, 'second'))

        then:
        1 * blogPost.setDescription(postDescription)

        then:
        1 * blogPostRepository.save(blogPost)
    }

    def "Should update only description if post already exists with different protocol"() {
        given:
        String postUrl = 'http://blogpost.com/blog'
        SyndEntry entry = mockSyndEntry(postUrl, postTitle, postDescription)
        BlogPost blogPost = Mock()
        RssEntryWithAuthor message = new RssEntryWithAuthor(Mock(Blog), entry)
        blogPostRepository.findByUrlEndingWith(removeHttpProtocolFrom(postUrl)) >> Option.of(blogPost)

        when:
        blogPostingActor.tell(message, ActorRef.noSender())
        testProbe.expectNoMsg(FiniteDuration.apply(1, 'second'))

        then:
        1 * blogPost.setDescription(postDescription)

        then:
        1 * blogPostRepository.save(blogPost)
    }

    def "Should use updatedDate if publishedDate is null"() {
        given:
        String postUrl = 'http://blogpost.com/blog'
        Date updatedDate = new Date()
        SyndEntry entry = mockSyndEntry(postUrl, postTitle, null, null, updatedDate)
        RssEntryWithAuthor message = new RssEntryWithAuthor(Mock(Blog), entry)
        blogPostRepository.findByUrlEndingWith(removeHttpProtocolFrom(postUrl)) >> Option.none()

        when:
        blogPostingActor.tell(message, ActorRef.noSender())
        testProbe.expectNoMsg(FiniteDuration.apply(1, 'second'))

        then:
        1 * blogPostFactory.create(postTitle, postUrl, toLocalDateTime(updatedDate), _ as Blog)
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

    private String removeHttpProtocolFrom(String link) {
        return link.replaceAll('^http', '')
    }

}
