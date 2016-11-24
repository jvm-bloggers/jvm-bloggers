package com.jvm_bloggers.admin_panel.blogs

import com.jvm_bloggers.MockSpringContextAwareSpecification
import com.jvm_bloggers.admin_panel.PaginationConfiguration
import com.jvm_bloggers.core.data_fetching.blog_posts.domain.BlogPostRepository
import com.jvm_bloggers.core.data_fetching.blogs.domain.Blog
import com.jvm_bloggers.core.data_fetching.blogs.domain.BlogRepository
import com.jvm_bloggers.core.data_fetching.blogs.domain.BlogType
import org.apache.wicket.request.mapper.parameter.PageParameters

import static com.jvm_bloggers.admin_panel.blogs.BlogPostsPage.BLOG_ID_PARAM
import static com.jvm_bloggers.admin_panel.blogs.BlogPostsPage.HEADER_ID
import static java.time.LocalDateTime.now


class BlogPostsPageSpec extends MockSpringContextAwareSpecification {

    private static final Long SAMPLE_BLOG_ID = 19L
    private static final String APOSTROPHE = "&#039;"

    BlogPostRepository blogPostRepository = Mock(BlogPostRepository.class)
    BlogRepository blogRepository = Mock(BlogRepository.class)
    PaginationConfiguration paginationConfiguration = new PaginationConfiguration(15);

    @Override
    protected void setupContext() {
        addBean(blogPostRepository)
        addBean(blogRepository)
        addBean(paginationConfiguration)
    }

    def "Should generate header from Blog's author"() {
        given:
            blogRepository.findOne(SAMPLE_BLOG_ID) >> sampleBlog()
            String expectedHeader = sampleBlog().author + APOSTROPHE + "s posts"
        when:
            tester.startPage(BlogPostsPage.class, new PageParameters().add(BLOG_ID_PARAM, SAMPLE_BLOG_ID.toString()))
        then:
            tester.assertLabel(HEADER_ID, expectedHeader)
    }

    def "Should generate exceptional header when no blog with given id found"() {
        given:
            blogRepository.findOne(SAMPLE_BLOG_ID) >> null
        when:
            tester.startPage(BlogPostsPage.class, new PageParameters().add(BLOG_ID_PARAM, SAMPLE_BLOG_ID.toString()))
        then:
            tester.assertLabel(HEADER_ID, "No such blog found")
    }

    private Blog sampleBlog() {
        Blog.builder()
                .id(1)
                .jsonId(2)
                .author("some author")
                .rss("some rss")
                .url("some url")
                .twitter("some twitter")
                .dateAdded(now())
                .blogType(BlogType.PERSONAL)
                .active(true)
                .build()
    }
}
