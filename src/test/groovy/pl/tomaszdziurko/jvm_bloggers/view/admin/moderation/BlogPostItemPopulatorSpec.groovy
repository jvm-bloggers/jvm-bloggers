package pl.tomaszdziurko.jvm_bloggers.view.admin.moderation

import org.apache.wicket.behavior.AttributeAppender
import org.apache.wicket.markup.repeater.Item
import org.apache.wicket.protocol.http.WebApplication
import org.apache.wicket.util.tester.WicketTester
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.test.util.ReflectionTestUtils

import pl.tomaszdziurko.jvm_bloggers.MockSpringContextAwareSpecification;
import pl.tomaszdziurko.jvm_bloggers.SpringContextAwareSpecification
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPostRepository;
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.BlogType;
import pl.tomaszdziurko.jvm_bloggers.utils.NowProvider
import spock.lang.Specification
import spock.lang.Subject

import java.time.LocalDateTime
import java.time.Month

class BlogPostItemPopulatorSpec extends MockSpringContextAwareSpecification {

    static LocalDateTime SATURDAY_19TH_12_00 = LocalDateTime.of(2016, Month.MARCH, 19, 12, 0, 0)

    NowProvider nowProvider = Stub(NowProvider) {
        now() >> SATURDAY_19TH_12_00
    }

    @Subject
    private BlogPostItemPopulator blogPostItemPopulator = new BlogPostItemPopulator(nowProvider)

    def "Should highlight post going in newsletter"() {
        given:
            BlogPost blogPost = createBlogPostPublishedOn(SATURDAY_19TH_12_00.plusDays(1))
            Item<BlogPost> item = createBlogPostItem(blogPost)
        when:
            blogPostItemPopulator.populateItem(item, null, null)
        then:
            item.getBehaviors(AttributeAppender).any { isHighlighted(it) }
    }

    def "Should not highlight post not going in newsletter"() {
        given:
            BlogPost blogPost = createBlogPostPublishedOn(SATURDAY_19TH_12_00.minusDays(1))
            Item<BlogPost> item = createBlogPostItem(blogPost)
        when:
            blogPostItemPopulator.populateItem(item, null, null)
        then:
            !(item.getBehaviors(AttributeAppender).any { isHighlighted(it) })
    }

    
    private boolean isHighlighted(AttributeAppender attributeAppender) {
        return attributeAppender.getAttribute() == "class" && attributeAppender.replaceModel.getObject() == "highlighted-post"
    }

    private BlogPost createBlogPostPublishedOn(LocalDateTime publicationDate) {
        Blog blog = Blog.builder()
                .jsonId(0L)
                .author("Blog author")
                .rss("rss")
                .url("url")
                .dateAdded(nowProvider.now())
                .blogType(BlogType.PERSONAL)
                .build()
        return BlogPost.builder()
                .blog(blog)
                .publishedDate(publicationDate)
                .title("title")
                .url("url")
                .build()
    }

    private Item<BlogPost> createBlogPostItem(BlogPost blogPost) {
        return new Item<>("itemId", 1, new BlogPostModel(blogPost))
    }

    @Override
    protected void setupContext() {
        addBean(Stub(BlogPostRepository))
    }
}
