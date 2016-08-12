package pl.tomaszdziurko.jvm_bloggers.rest.blogpost_redirect

import akka.actor.ActorSystem
import org.springframework.test.web.servlet.MockMvc
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPost
import pl.tomaszdziurko.jvm_bloggers.blog_posts.domain.BlogPostRepository
import pl.tomaszdziurko.jvm_bloggers.click_counter.domain.ClickRepository
import pl.tomaszdziurko.jvm_bloggers.utils.NowProvider
import spock.lang.Specification

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.header
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup

class RedirectControllerSpec extends Specification {

    private BlogPostRepository blogPostRepositoryMock = Mock(BlogPostRepository)
    private MockMvc mockMvc = standaloneSetup(
            new RedirectController(
                    blogPostRepositoryMock,
                    Stub(ClickRepository),
                    ActorSystem.create("test"),
                    Stub(NowProvider)))
            .build()

    def "Should redirect to blogpost url"() {
        given:
            final String uid = '1234'
            final String url = 'http://www.blog.com/post'
        and:
            blogPostRepositoryMock.findByUid(uid) >> Optional.of(new BlogPost(url: url))
        expect:
            mockMvc.perform(get("/r/$uid"))
                .andExpect(status().isFound())
                .andExpect(header().string('Location',
                    "$url?utm_source=jvm-bloggers.com&utm_medium=link&utm_campaign=jvm-bloggers"))
    }

    def "Should return 404 when blogpost does not exist"() {
        given:
            final String uid = '1234'
        and:
            blogPostRepositoryMock.findByUid(uid) >> Optional.empty()
        expect:
            mockMvc.perform(get("/r/$uid"))
                .andExpect(status().isNotFound())
    }
}
