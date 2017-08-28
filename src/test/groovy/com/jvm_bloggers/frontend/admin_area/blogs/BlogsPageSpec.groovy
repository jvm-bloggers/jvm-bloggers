package com.jvm_bloggers.frontend.admin_area.blogs

import com.jvm_bloggers.MockSpringContextAwareSpecification
import com.jvm_bloggers.entities.blog.Blog
import com.jvm_bloggers.entities.blog.BlogRepository
import com.jvm_bloggers.entities.blog.BlogType
import com.jvm_bloggers.entities.blog_post.BlogPostRepository
import com.jvm_bloggers.frontend.admin_area.PaginationConfiguration

import static com.jvm_bloggers.frontend.admin_area.blogs.BlogsPage.BLOGS_DATA_VIEW_ID
import static com.jvm_bloggers.frontend.admin_area.blogs.BlogsPage.BLOG_DATA_FORM_ID
import static com.jvm_bloggers.frontend.admin_area.blogs.BlogsPage.BLOG_POSTS_LINK_ID
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
            .bookmarkableId("bookmarkableId")
            .author("some author")
            .rss("some rss")
            .url("some url")
            .twitter("some twitter")
            .dateAdded(now())
            .blogType(BlogType.PERSONAL)
            .active(true)
            .moderationRequired(false)
            .build()
    }

    private def String linkPath(String buttonId) {
        [BLOG_DATA_FORM_ID, BLOGS_DATA_VIEW_ID, 1, buttonId].join(":")
    }

}
