package com.jvm_bloggers.core.data_fetching.blog_posts

import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import akka.testkit.JavaTestKit
import com.jvm_bloggers.entities.blog.Blog
import com.jvm_bloggers.entities.blog_post.BlogPost
import com.jvm_bloggers.entities.blog_post.BlogPostRepository
import com.jvm_bloggers.entities.tag.Tag
import com.jvm_bloggers.entities.tag.TagRepository
import com.rometools.rome.feed.synd.SyndCategory
import com.rometools.rome.feed.synd.SyndCategoryImpl
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
    TagRepository tagRepository;

    String postTitle = 'Title'
    String postDescription = 'description'

    def setup() {
        ActorSystem system = ActorSystem.create('test')
        testProbe = new JavaTestKit(system)
        blogPostRepository = Mock(BlogPostRepository)
        blogPostFactory = Mock(BlogPostFactory)
        tagRepository = Mock(TagRepository)
        Props props = NewBlogPostStoringActor.props(blogPostRepository, blogPostFactory, tagRepository)
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
        SyndEntry entry = mockSyndEntry(postUrl, postTitle, postDescription, [createSyndCategory("JAVA")])
        RssEntryWithAuthor message = new RssEntryWithAuthor(blog, entry)
        blogPost.getTags() >> Collections.emptySet()
        blogPostFactory.create(postTitle, postUrl, toLocalDateTime(entry.getPublishedDate()), blog) >> blogPost
        blogPostRepository.findByUrlEndingWith(removeHttpProtocolFrom(postUrl)) >> Option.none()
        tagRepository.findByTag("java") >> Option.of(new Tag(id: 1, tag: "java"))
        when:
        blogPostingActor.tell(message, ActorRef.noSender())
        testProbe.expectNoMsg(FiniteDuration.apply(1, 'second'))

        then:
        blogPost.setTags(Collections.singleton(new Tag(id: 1, tag: "java")))

        then:
        1 * blogPostRepository.save(blogPost)
    }

    def "Should not persist blog post with invalid URL"() {
        given:
        String invalidLink = 'invalidLink'
        SyndEntry entry = mockSyndEntry(invalidLink, postTitle, postDescription, Collections.emptyList())
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
        SyndEntry entry = mockSyndEntry(postUrl, postTitle, postDescription, Collections.emptyList())
        BlogPost blogPost = Mock()
        blogPost.getTags() >> Collections.emptySet()
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

    def "should update Tags if post already exists"() {
        given:
        String postUrl = 'http://blogpost.com/blog'
        List categories = [createSyndCategory("Java"), createSyndCategory("SPRING")]
        SyndEntry entry = mockSyndEntry(postUrl, postTitle, postDescription, categories)
        BlogPost blogPost = Mock()
        blogPost.getTags() >> [new Tag(id: 1, tag:'java')]
        RssEntryWithAuthor message = new RssEntryWithAuthor(Mock(Blog), entry)
        blogPostRepository.findByUrlEndingWith(removeHttpProtocolFrom(postUrl)) >> Option.of(blogPost)
        tagRepository.findByTag('java')  >> Option.of(new Tag(id: 1, tag: 'java'));
        tagRepository.findByTag('spring')  >> Option.none()

        when:
        blogPostingActor.tell(message, ActorRef.noSender())
        testProbe.expectNoMsg(FiniteDuration.apply(1, 'second'))

        then:
        1 * blogPost.setTags(new HashSet<>([new Tag(id : 1, tag: "java"), new Tag(tag: "spring")]))

        then:
        1 * blogPostRepository.save(blogPost)

    }

    def "Should update only description if post already exists with different protocol"() {
        given:
        String postUrl = 'http://blogpost.com/blog'
        SyndEntry entry = mockSyndEntry(postUrl, postTitle, postDescription, Collections.emptyList())
        BlogPost blogPost = Mock()
        blogPost.tags >> Collections.emptySet()
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
        SyndEntry entry = mockSyndEntry(postUrl, postTitle, null, null, updatedDate, null)
        RssEntryWithAuthor message = new RssEntryWithAuthor(Mock(Blog), entry)
        blogPostRepository.findByUrlEndingWith(removeHttpProtocolFrom(postUrl)) >> Option.none()

        when:
        blogPostingActor.tell(message, ActorRef.noSender())
        testProbe.expectNoMsg(FiniteDuration.apply(1, 'second'))

        then:
        1 * blogPostFactory.create(postTitle, postUrl, toLocalDateTime(updatedDate), _ as Blog)
    }

    private SyndEntry mockSyndEntry(String postUrl, String postTitle, String postDescription, List categories) {
        return mockSyndEntry(postUrl, postTitle, postDescription, new Date(), new Date(), categories)
    }

    private SyndEntry mockSyndEntry(String postUrl, String postTitle, String postDescription, Date publishedDate, Date updatedDate, List categories) {
        SyndEntry entry = Mock(SyndEntry)
        entry.getPublishedDate() >> publishedDate
        entry.getUpdatedDate() >> updatedDate
        entry.getLink() >> postUrl
        entry.getTitle() >> postTitle
        entry.getDescription() >> Stub(SyndContent) {
            getValue() >> postDescription
        }
        entry.getCategories()  >> categories
        return entry
    }

    private String removeHttpProtocolFrom(String link) {
        return link.replaceAll('^http', '')
    }

    private SyndCategory createSyndCategory(String tag) {
        return new SyndCategoryImpl(name: tag);
    }

}
