package pl.tomaszdziurko.jvm_bloggers.rest.newsletter_issue

import org.hamcrest.Matchers
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.test.SpringApplicationContextLoader
import org.springframework.test.context.ActiveProfiles
import org.springframework.test.context.ContextConfiguration
import org.springframework.test.context.web.WebAppConfiguration
import org.springframework.test.web.servlet.MockMvc
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders
import org.springframework.test.web.servlet.result.MockMvcResultMatchers
import org.springframework.test.web.servlet.setup.ConfigurableMockMvcBuilder
import org.springframework.test.web.servlet.setup.MockMvcBuilders
import org.springframework.web.context.WebApplicationContext
import pl.tomaszdziurko.jvm_bloggers.JvmBloggersApplication
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPostRepository
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.Blog
import pl.tomaszdziurko.jvm_bloggers.blogs.domain.BlogRepository
import pl.tomaszdziurko.jvm_bloggers.newsletter_issues.domain.NewsletterIssue
import pl.tomaszdziurko.jvm_bloggers.newsletter_issues.domain.NewsletterIssueRepository
import spock.lang.Specification

import java.time.LocalDate
import java.time.LocalDateTime

import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status
import static pl.tomaszdziurko.jvm_bloggers.blogs.domain.BlogType.PERSONAL
import static pl.tomaszdziurko.jvm_bloggers.rest.ContentTypes.JVM_BLOGGERS_V1

@ContextConfiguration(classes = [JvmBloggersApplication], loader = SpringApplicationContextLoader)
@WebAppConfiguration
@ActiveProfiles("test")
class NewsletterIssueControllerIntegrationSpec extends Specification {

    MockMvc mockMvc

    @Autowired
    BlogRepository blogRepository

    @Autowired
    BlogPostRepository blogPostRepository

    @Autowired
    NewsletterIssueRepository newsletterIssueRepository

    @Autowired
    WebApplicationContext webApplicationContext

    def setup() {
        ConfigurableMockMvcBuilder mockMvcBuilder = MockMvcBuilders.webAppContextSetup(webApplicationContext)
        mockMvc = mockMvcBuilder.build()
    }

    def "Should return latest issue"() {
        given:
            NewsletterIssue issue = prepareNewsletterIssue()
        expect:
            mockMvc.perform(MockMvcRequestBuilders
                    .get("/issues/latest")
                    .contentType(JVM_BLOGGERS_V1)
                    .accept(JVM_BLOGGERS_V1)
            ).andExpect(status().isOk())
             .andExpect(MockMvcResultMatchers.jsonPath('$.number', Matchers. is(issue.getIssueNumber().toInteger())))
             .andExpect(MockMvcResultMatchers.jsonPath('$.heading', Matchers.is(issue.heading)))
             .andExpect(MockMvcResultMatchers.jsonPath('$.posts[0].title', Matchers.is(issue.blogPosts.first().getTitle())))
    }

    def "Should return 404 when latest issue does not exist"() {
        given:
            newsletterIssueRepository.deleteAll()
        expect:
            mockMvc.perform(MockMvcRequestBuilders
                    .get("/issues/latest")
                    .contentType(JVM_BLOGGERS_V1)
                    .accept(JVM_BLOGGERS_V1)
            ).andExpect(status().isNotFound())
    }

    private NewsletterIssue prepareNewsletterIssue() {
        Blog blog = persistBlog()
        BlogPost blogPost = persistBlogPost(blog)
        NewsletterIssue newsletterIssue = new NewsletterIssue(
                2, LocalDate.now(), [blog], [blogPost], "Some heading", "Some varia"
        )
        return newsletterIssueRepository.save(newsletterIssue)
    }


    private Blog persistBlog() {
        return blogRepository.save(
                Blog.builder()
                        .jsonId(1L)
                        .author("Some Author")
                        .rss("http://someblog.pl/rss")
                        .dateAdded(LocalDateTime.now())
                        .blogType(PERSONAL)
                        .build()
        );
    }

    private BlogPost persistBlogPost(final Blog blog) {
        return blogPostRepository.save(BlogPost.builder()
                .publishedDate(LocalDateTime.now())
                .approved(true)
                .blog(blog)
                .title("Some title")
                .url("Some url")
                .build()
        );
    }

}

