package pl.tomaszdziurko.jvm_bloggers.view.admin.moderation

import org.apache.wicket.markup.repeater.Item
import org.apache.wicket.protocol.http.WebApplication
import org.apache.wicket.util.tester.WicketTester
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.test.util.ReflectionTestUtils
import pl.tomaszdziurko.jvm_bloggers.SpringContextAwareSpecification
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog
import pl.tomaszdziurko.jvm_bloggers.utils.NowProvider
import spock.lang.Subject

import java.time.LocalDateTime
import java.time.Month

class BlogPostItemPopulatorSpec extends SpringContextAwareSpecification {

    private WicketTester tester

    static LocalDateTime SATURDAY_19TH_12_00 = LocalDateTime.of(2016, Month.MARCH, 19, 12, 0, 0)

    @Autowired
    private WebApplication wicketApplication

    @Subject
    private BlogPostItemPopulator blogPostItemPopulator = new BlogPostItemPopulator()

    private AttributeModifierWrapper attributeModifierWrapper;

    def setup() {
        NowProvider nowProvider = Stub(NowProvider) {
            now() >> SATURDAY_19TH_12_00
        }
        ReflectionTestUtils.setField(blogPostItemPopulator, "nowProvider", nowProvider)

        attributeModifierWrapper = Spy(AttributeModifierWrapper)
        ReflectionTestUtils.setField(blogPostItemPopulator, "attributeModifierWrapper", attributeModifierWrapper)

        // This is a workaround for problems with Spring Boot and WicketTester
        // https://issues.apache.org/jira/browse/WICKET-6053 and
        // https://github.com/MarcGiffing/wicket-spring-boot/issues/31
        ReflectionTestUtils.setField(wicketApplication, "name", null)
        tester = new WicketTester(wicketApplication)
    }

    def "Should highlight post going in newsletter"() {
        given:
            BlogPost blogPost = createBlogPostPublishedOn(SATURDAY_19TH_12_00.plusDays(1))
            Item<BlogPost> item = createBlogPostItem(blogPost)
        when:
            blogPostItemPopulator.populateItem(item, null, null)
        then:
            1 * attributeModifierWrapper.append("class", "highlighted-post")
    }

    private BlogPost createBlogPostPublishedOn(LocalDateTime publicationDate) {
        Blog blog = Blog.builder()
                .author("Blog author")
                .build()
        return BlogPost.builder()
                .blog(blog)
                .publishedDate(publicationDate)
                .build()
    }

    private Item<BlogPost> createBlogPostItem(BlogPost blogPost) {
        BlogPostModel blogPostModel = new BlogPostModel(blogPost)
        return new Item<>("itemId", 1, blogPostModel)
    }

    def "Should not highlight post not going in newsletter"() {
        given:
            BlogPost blogPost = createBlogPostPublishedOn(SATURDAY_19TH_12_00.minusDays(1))
            Item<BlogPost> item = createBlogPostItem(blogPost)
        when:
            blogPostItemPopulator.populateItem(item, null, null)
        then:
            0 * attributeModifierWrapper.append("class", "highlighted-post")
    }
}
