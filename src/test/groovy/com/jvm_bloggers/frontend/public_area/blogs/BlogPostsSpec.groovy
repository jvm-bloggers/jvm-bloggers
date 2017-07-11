package com.jvm_bloggers.frontend.public_area.blogs

import com.google.common.collect.Lists
import com.jvm_bloggers.MockSpringContextAwareSpecification
import com.jvm_bloggers.entities.blog.Blog
import com.jvm_bloggers.entities.blog.BlogRepository
import com.jvm_bloggers.entities.blog_post.BlogPost
import com.jvm_bloggers.entities.blog_post.BlogPostRepository
import com.jvm_bloggers.frontend.admin_area.PaginationConfiguration
import com.jvm_bloggers.frontend.common_components.infinite_scroll.InfinitePaginationPanel
import com.jvm_bloggers.frontend.public_area.common_layout.RightFrontendSidebarBackingBean
import com.jvm_bloggers.utils.NowProvider
import org.apache.wicket.markup.repeater.data.DataView
import org.apache.wicket.request.mapper.parameter.PageParameters

import java.time.LocalDateTime

import static com.jvm_bloggers.entities.blog.BlogType.PERSONAL
import static com.jvm_bloggers.frontend.public_area.blogs.BlogPostsPage.AUTHOR_ID
import static com.jvm_bloggers.frontend.public_area.blogs.BlogPostsPage.BLOG_ID_PARAM
import static com.jvm_bloggers.frontend.public_area.blogs.BlogPostsPage.DATA_VIEW_ID
import static com.jvm_bloggers.frontend.public_area.blogs.BlogPostsPage.DATA_VIEW_WRAPPER_ID
import static com.jvm_bloggers.frontend.public_area.blogs.BlogPostsPage.INFINITE_SCROLL_ID
import static java.util.UUID.randomUUID

class BlogPostsSpec extends MockSpringContextAwareSpecification {

    BlogRepository blogRepository = Mock()
    BlogPostRepository blogPostRepository = Mock()
    NowProvider nowProvider = Stub() { now() >> LocalDateTime.now() }
    PaginationConfiguration paginationConfiguration = new PaginationConfiguration(5)
    RightFrontendSidebarBackingBean sidebarBackingBean = Stub()

    @Override
    protected void setupContext() {
        addBean(blogRepository)
        addBean(blogPostRepository)
        addBean(nowProvider)
        addBean(paginationConfiguration)
        addBean(sidebarBackingBean)
    }

    def "Should render blog posts list"() {
        given:
        Long blogId = 99
        List blogPosts = Lists.asList(
                aBlogPost("url", "title", nowProvider.now()),
                aBlogPost("url", "title", nowProvider.now().minusDays(1)),
                aBlogPost("url", "title", nowProvider.now().minusDays(2))
        )
        blogPostRepository.findByBlogIdOrderByPublishedDateDesc(blogId, _) >> blogPosts
        blogPostRepository.countByBlogId(blogId) >> blogPosts.size()

        when:
        tester.startPage(BlogPostsPage, new PageParameters().add(BLOG_ID_PARAM, blogId))

        then:
        tester.assertComponent("$DATA_VIEW_WRAPPER_ID:$DATA_VIEW_ID", DataView)
        tester.assertComponent("$DATA_VIEW_WRAPPER_ID:$INFINITE_SCROLL_ID", InfinitePaginationPanel)
    }

    def "Should render page header with author name"() {
        given:
        Blog blog = aBlog("Bruce Wayne")
        Long blogId = 99
        List blogPosts = Lists.asList(
                aBlogPost("url", "title", nowProvider.now()),
                aBlogPost("url", "title", nowProvider.now().minusDays(1)),
                aBlogPost("url", "title", nowProvider.now().minusDays(2))
        )
        blogRepository.findOne(blogId) >> blog
        blogPostRepository.findByBlogIdOrderByPublishedDateDesc(blogId, _) >> blogPosts
        blogPostRepository.countByBlogId(blogId) >> blogPosts.size()

        when:
        tester.startPage(BlogPostsPage, new PageParameters().add(BLOG_ID_PARAM, blogId))

        then:
        tester.assertLabel("$AUTHOR_ID", blog.author)
    }

    def "Should generate next-link for infinity scroll"() {
        given:
        Long blogId = 99
        List blogPosts = Lists.asList(
                aBlogPost("url", "title", nowProvider.now()),
                aBlogPost("url", "title", nowProvider.now().minusDays(1)),
                aBlogPost("url", "title", nowProvider.now().minusDays(2))
        )
        blogPostRepository.findByBlogIdOrderByPublishedDateDesc(blogId, _) >> blogPosts
        blogPostRepository.countByBlogId(blogId) >> blogPosts.size()

        when:
        tester.startPage(BlogPostsPage, new PageParameters().add(BLOG_ID_PARAM, blogId))

        then:
        tester.getTagByWicketId("next-page").getAttributeContains("href", "BlogPostsPage?-1.0")
    }

    private Blog aBlog(String author) {
        new Blog.BlogBuilder()
        .url(randomUUID().toString())
        .jsonId(1)
        .rss(randomUUID().toString())
        .dateAdded(nowProvider.now())
        .author(author)
        .blogType(PERSONAL)
        .build()
    }

    private BlogPost aBlogPost(String url, String title, LocalDateTime publishedDate) {
        new BlogPost.BlogPostBuilder()
        .url(url)
        .blog(new Blog())
        .publishedDate(publishedDate)
        .title(title)
        .build()
    }
}
