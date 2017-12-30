package com.jvm_bloggers.frontend.admin_area.blogs

import com.jvm_bloggers.MockSpringContextAwareSpecification
import com.jvm_bloggers.entities.blog.Blog
import com.jvm_bloggers.entities.blog.BlogRepository
import com.jvm_bloggers.entities.blog.BlogType
import com.jvm_bloggers.entities.blog_post.BlogPostRepository
import com.jvm_bloggers.frontend.admin_area.PaginationConfiguration
import org.apache.wicket.util.tester.TagTester
import org.springframework.data.domain.PageImpl

import static com.jvm_bloggers.frontend.admin_area.blogs.BlogsPage.*
import static java.time.LocalDateTime.now

class BlogsPageSpec extends MockSpringContextAwareSpecification {

    PaginationConfiguration paginationConfiguration = new PaginationConfiguration(15)
    BlogRepository blogRepository = Mock(BlogRepository.class)

    @Override
    protected void setupContext() {
        addBean(paginationConfiguration)
        addBean(blogRepository)
        addBean(Mock(BlogPostRepository.class))
    }

    def "Should redirect to blog's posts page"() {
        given:
            Blog blog = sampleBlog(1)
            blogRepository.findAll(_) >> new PageImpl<>([blog])
            blogRepository.count() >> 1

        when:
            tester.startPage(BlogsPage.class)
            tester.clickLink(linkPath(1, 7, POSTS_LINK))

        then:
            tester.assertRenderedPage(BlogPostsPage.class)
    }

    def "Should be ordered by author asc"() {
        given:
            Blog blog1 = sampleBlog(1)
            Blog blog2 = sampleBlog(2)
            blogRepository.findAll(_) >> new PageImpl<>([blog1, blog2])
            blogRepository.count() >> 2

        when:
            tester.startPage(BlogsPage.class)

        then:
            "Author" == getSortedColumnHeader("wicket_orderUp")
    }

    def "Should be ordered by author desc"() {
        given:
            Blog blog1 = sampleBlog(1)
            Blog blog2 = sampleBlog(2)
            blogRepository.findAll(_) >> new PageImpl<>([blog1, blog2])
            blogRepository.count() >> 2

        when:
            tester.startPage(BlogsPage.class)
            tester.clickLink(headerLinkPath(1))

        then:
            "Author" == getSortedColumnHeader("wicket_orderDown")
    }

    private String getSortedColumnHeader(String sortingClass) {
        TagTester authorHeaderTester = TagTester.createTagByAttribute(
                tester.getLastResponse().getDocument(), "class", sortingClass)
        return authorHeaderTester.getChild("a").getChild("span").getValue()
    }

    private Blog sampleBlog(int id) {
        Blog.builder()
                .id(id)
                .bookmarkableId("bookmarkableId " + id)
                .author("some author " + id)
                .rss("some rss " + id)
                .url("some url")
                .twitter("some twitter")
                .dateAdded(now())
                .blogType(BlogType.PERSONAL)
                .active(true)
                .moderationRequired(false)
                .build()
    }

    private String headerLinkPath(int column) {
        [BLOG_DATA_FORM_ID, BLOGS_DATA_TABLE_ID, "topToolbars:toolbars:1:headers", column, "header:orderByLink"].join(":")
    }

    private String linkPath(int row, int column, String... ids) {
        [BLOG_DATA_FORM_ID, BLOGS_DATA_TABLE_ID, "body:rows", row, "cells", column, "cell", ids].flatten().join(":")
    }

}
