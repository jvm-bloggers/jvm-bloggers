package com.jvm_bloggers.core.blogpost_redirect

import akka.actor.ActorSystem
import com.jvm_bloggers.entities.blog_post.BlogPost
import com.jvm_bloggers.entities.blog_post.BlogPostRepository
import com.jvm_bloggers.entities.click.ClickRepository
import com.jvm_bloggers.utils.NowProvider
import io.vavr.control.Option
import org.springframework.test.web.servlet.MockMvc
import spock.lang.Specification
import spock.lang.Subject

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.header
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup

@Subject(RedirectController)
class RedirectControllerSpec extends Specification {

    private static final String homePageUrl = 'http://jvm-bloggers.com'

    private BlogPostRepository blogPostRepositoryMock = Mock(BlogPostRepository)
    private MockMvc mockMvc = standaloneSetup(
            new RedirectController(
                    blogPostRepositoryMock,
                    Stub(ClickRepository),
                    ActorSystem.create('test'),
                    homePageUrl,
                    Stub(NowProvider)))
            .build()

    def "Should redirect to blogpost url"() {
        given:
            final String uid = '1234'
            final String url = 'http://www.blog.com/post'
        and:
            blogPostRepositoryMock.findByUid(uid) >> Option.of(new BlogPost(url: url))
        expect:
            mockMvc.perform(get("/r/$uid"))
                .andExpect(status().isFound())
                .andExpect(header().string('Location',
                    "$url?utm_source=jvm-bloggers.com&utm_medium=link&utm_campaign=jvm-bloggers"))
    }

    def "Should redirect to the home page when blogpost does not exist"() {
        given:
            final String uid = '1234'
        and:
            blogPostRepositoryMock.findByUid(uid) >> Option.none()
        expect:
        mockMvc.perform(get("/r/$uid"))
            .andExpect(status().isFound())
            .andExpect(header().string('Location', homePageUrl))
    }
    
}
