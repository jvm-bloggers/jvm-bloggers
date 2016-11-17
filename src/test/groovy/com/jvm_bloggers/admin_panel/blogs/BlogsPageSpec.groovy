package com.jvm_bloggers.admin_panel.blogs

import com.jvm_bloggers.MockSpringContextAwareSpecification
import com.jvm_bloggers.admin_panel.PaginationConfiguration
import com.jvm_bloggers.core.data_fetching.blog_posts.domain.BlogPostRepository
import com.jvm_bloggers.core.data_fetching.blogs.domain.Blog
import com.jvm_bloggers.core.data_fetching.blogs.domain.BlogRepository
import com.jvm_bloggers.core.data_fetching.blogs.domain.BlogType
import org.apache.wicket.model.Model

import static com.jvm_bloggers.admin_panel.blogs.BlogsPage.*
import static java.time.LocalDateTime.now

class BlogsPageSpec extends MockSpringContextAwareSpecification {

    PaginationConfiguration paginationConfiguration = new PaginationConfiguration(15);
    BlogsPageRequestHandler requestHandler = Mock(BlogsPageRequestHandler.class)

    @Override
    protected void setupContext() {
        addBean(paginationConfiguration)
        addBean(requestHandler)
        addBean(Mock(BlogRepository.class))
        addBean(Mock(BlogPostRepository.class))
    }

    def "Should redirect to blog's posts page"() {
        given:
            Blog blog = sampleBlog()
            requestHandler.iterator(_, _) >> [blog].iterator()
            requestHandler.size() >> 1
            requestHandler.model(_) >> new BlogModel(blog)
        when:
            tester.startPage(BlogsPage.class)
            tester.clickLink(linkPath(BLOG_POSTS_LINK_ID))
        then:
            tester.assertRenderedPage(BlogPostsPage.class)
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

    private def String linkPath(String buttonId) {
        [BLOG_DATA_FORM_ID, BLOGS_DATA_VIEW_ID, 1, buttonId].join(":")
    }

}
