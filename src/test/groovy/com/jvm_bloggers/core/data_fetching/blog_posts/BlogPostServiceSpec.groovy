package com.jvm_bloggers.core.data_fetching.blog_posts

import com.jvm_bloggers.SpringContextAwareSpecification
import com.jvm_bloggers.entities.blog.Blog
import com.jvm_bloggers.entities.blog.BlogRepository
import com.jvm_bloggers.entities.blog.BlogType
import com.jvm_bloggers.entities.blog_post.BlogPost
import com.jvm_bloggers.entities.blog_post.BlogPostRepository
import com.jvm_bloggers.entities.tag.TagRepository
import com.jvm_bloggers.utils.DateTimeUtilities
import com.rometools.rome.feed.synd.SyndCategory
import com.rometools.rome.feed.synd.SyndCategoryImpl
import com.rometools.rome.feed.synd.SyndContent
import com.rometools.rome.feed.synd.SyndEntry
import io.vavr.control.Option
import org.springframework.beans.factory.annotation.Autowired
import spock.lang.Subject

import java.time.LocalDateTime

import static java.util.Collections.emptyList
import static java.util.Collections.emptySet

@Subject(BlogPostService)
class BlogPostServiceSpec extends SpringContextAwareSpecification {

    @Autowired
    BlogPostRepository blogPostRepository

    @Autowired
    BlogPostFactory blogPostFactory

    @Autowired
    BlogRepository blogRepository;

    @Autowired
    TagRepository tagRepository

    @Autowired
    BlogPostService blogPostService

    String blogBookmarkableId = "some-bookmarkable-id"
    String postTitle = 'Title'
    String postDescription = 'Description'

    def setup() {
        if (blogRepository.findByBookmarkableId(blogBookmarkableId).isEmpty()) {
            Blog blog = new Blog(null, blogBookmarkableId, "Test Author", "rss address", "blog url",
                    "@twitterHandle", LocalDateTime.now(), BlogType.PERSONAL, true, false)
            blogRepository.save(blog)
        }
    }

    def "Should persist new blog posts"() {
        given:
        String postUrl = 'http://blogpost.com/blog' + UUID.randomUUID()

        SyndEntry entry = mockSyndEntry(postUrl, postTitle, postDescription, [createSyndCategory("java")])
        RssEntryWithAuthor message = new RssEntryWithAuthor(findTestBlog(), entry)

        when:
        def post1 = blogPostService.addOrUpdate(message).get()

        then:
        def reloadedPost1 = blogPostRepository.findById(post1.getId()).get()
        reloadedPost1.getTitle() == postTitle
        reloadedPost1.getTags().size() == 1

        and:
        SyndEntry entry2 = mockSyndEntry(postUrl + "part2", "Another Post", postDescription, [createSyndCategory("java"), createSyndCategory("testing")])
        RssEntryWithAuthor message2 = new RssEntryWithAuthor(findTestBlog(), entry2)

        when:
        def post2 = blogPostService.addOrUpdate(message2).get()

        then:
        def reloadedPost2 = blogPostRepository.findById(post2.getId()).get()
        reloadedPost2.getTitle() == "Another Post"
        reloadedPost2.getTags().collect {it.value} == ["java", "testing"]
    }

    def "Should not persist blog post with invalid URL"() {
        given:
        String invalidLink = 'invalidLink'
        SyndEntry entry = mockSyndEntry(invalidLink, postTitle, postDescription, emptyList())
        RssEntryWithAuthor message = new RssEntryWithAuthor(findTestBlog(), entry)

        when:
        Option<BlogPost> post = blogPostService.addOrUpdate(message)

        then:
        post.isEmpty()
    }

    def "Should update description if post already exists"() {
        given:
        String postUrl = 'http://blogpost.com/blog' + UUID.randomUUID()
        SyndEntry entry = mockSyndEntry(postUrl, postTitle, "", emptyList())
        RssEntryWithAuthor message = new RssEntryWithAuthor(findTestBlog(), entry)
        def newBlogPost = blogPostService.addOrUpdate(message).get()

        when:
        SyndEntry entryWithUpdatedDescription = mockSyndEntry(postUrl, postTitle, postDescription, emptyList())
        RssEntryWithAuthor updatedRssEntryMessage = new RssEntryWithAuthor(findTestBlog(), entryWithUpdatedDescription)
        blogPostService.addOrUpdate(updatedRssEntryMessage)

        then:
        def updatedBlogPost = blogPostRepository.findById(newBlogPost.getId()).get()
        updatedBlogPost.getDescription() == postDescription
    }

    def "Should update tags if post already exists"() {
        given:
        String postUrl = 'http://blogpost.com/blog' + UUID.randomUUID()
        SyndEntry entry = mockSyndEntry(postUrl, postTitle, postDescription, [createSyndCategory("conference")])
        RssEntryWithAuthor message = new RssEntryWithAuthor(findTestBlog(), entry)
        def newBlogPost = blogPostService.addOrUpdate(message).get()

        when:
        SyndEntry entryWithUpdatedDescription = mockSyndEntry(postUrl, postTitle, postDescription, [createSyndCategory("java"), createSyndCategory("geecon")])
        RssEntryWithAuthor updatedRssEntryMessage = new RssEntryWithAuthor(findTestBlog(), entryWithUpdatedDescription)
        blogPostService.addOrUpdate(updatedRssEntryMessage)

        then:
        def updatedBlogPost = blogPostRepository.findById(newBlogPost.getId()).get()
        updatedBlogPost.getTags().collect({it.value}).sort() == ["java", "geecon"].sort()

        and:
        tagRepository.findByValue("conference").get().posts.isEmpty()
    }

    def "Should update only description if post already exists with different protocol"() {
        given:
        String postUrlWithoutProtocol = "blogpost.com/blog-entry"
        String newDescription = "A new description"
        SyndEntry entry = mockSyndEntry("http://" + postUrlWithoutProtocol, postTitle, postDescription, emptyList())

        RssEntryWithAuthor message = new RssEntryWithAuthor(findTestBlog(), entry)
        def newBlogPost = blogPostService.addOrUpdate(message).get()

        when:
        SyndEntry entryWitHttps = mockSyndEntry("https://" + postUrlWithoutProtocol, postTitle, newDescription, emptyList())
        RssEntryWithAuthor updatedMessage = new RssEntryWithAuthor(findTestBlog(), entryWitHttps)
        def updatedBlogPost = blogPostService.addOrUpdate(updatedMessage).get()

        then:
        updatedBlogPost.getId() == newBlogPost.getId()
        def reloadedBlogPost = blogPostRepository.findById(newBlogPost.getId()).get()
        reloadedBlogPost.getDescription() == newDescription
    }

    def "Should use updatedDate if publishedDate is null"() {
        given:
        String postUrl = 'http://blogpost.com/blog' + UUID.randomUUID()
        Date updatedDate = new Date()
        SyndEntry entry = mockSyndEntry(postUrl, postTitle, postDescription, null, updatedDate, emptyList())
        RssEntryWithAuthor message = new RssEntryWithAuthor(findTestBlog(), entry)

        when:
        def blogPost = blogPostService.addOrUpdate(message).get()

        then:
        blogPost.getPublishedDate() == DateTimeUtilities.toLocalDateTime(updatedDate)
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
        entry.getCategories() >> categories
        return entry
    }

    private SyndCategory createSyndCategory(String tag) {
        return new SyndCategoryImpl(name: tag);
    }

    private Blog findTestBlog() {
        blogRepository.findByBookmarkableId(blogBookmarkableId).get()
    }

}
