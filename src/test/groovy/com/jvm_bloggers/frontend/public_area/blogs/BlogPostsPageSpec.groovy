package com.jvm_bloggers.frontend.public_area.blogs

import com.jvm_bloggers.MockSpringContextAwareSpecification
import com.jvm_bloggers.TestTimeProvider
import com.jvm_bloggers.domain.query.blog_post_for_listing.BlogDisplayDetails
import com.jvm_bloggers.domain.query.blog_post_for_listing.BlogPostForListing
import com.jvm_bloggers.frontend.common_components.infinite_scroll.InfinitePaginationPanel
import com.jvm_bloggers.frontend.public_area.blogs.single_blog.BlogPostsPage
import com.jvm_bloggers.frontend.public_area.blogs.single_blog.BlogPostsPageBackingBean
import com.jvm_bloggers.frontend.public_area.blogs.single_blog.BlogPostsPageRequestHandler
import com.jvm_bloggers.frontend.public_area.common_layout.RightFrontendSidebarBackingBean
import com.jvm_bloggers.utils.NowProvider
import io.vavr.control.Option
import org.apache.wicket.Component
import org.apache.wicket.markup.html.basic.Label
import org.apache.wicket.markup.html.link.ExternalLink
import org.apache.wicket.markup.repeater.data.DataView
import org.apache.wicket.model.Model
import org.apache.wicket.request.mapper.parameter.PageParameters

import java.time.LocalDateTime

import static com.jvm_bloggers.entities.blog.BlogType.COMPANY
import static com.jvm_bloggers.entities.blog.BlogType.PERSONAL
import static com.jvm_bloggers.frontend.WicketTestUtils.pathVia
import static com.jvm_bloggers.frontend.public_area.blogs.single_blog.BlogPostsPage.*
import static com.jvm_bloggers.utils.DateTimeUtilities.DATE_FORMATTER
import static java.util.UUID.randomUUID

class BlogPostsPageSpec extends MockSpringContextAwareSpecification {

    NowProvider nowProvider = new TestTimeProvider(LocalDateTime.now())
    BlogPostsPageBackingBean backingBean = GroovyMock()

    @Override
    protected void setupContext() {
        addBean(nowProvider)
        addBean(backingBean)
        addBean(Stub(AbstractBlogsPageBackingBean) {
            defaultPageSize() >> 1
        })
        addBean(Stub(RightFrontendSidebarBackingBean))
    }

    def "Should render blog posts list"() {
        given:
        String blogBookmarkableId = "blogBookmarkableId"
        Long blogId = 10
        BlogDisplayDetails blogDisplayDetails = new BlogDisplayDetails("author", PERSONAL, "url")
        backingBean.defaultPageSize() >> 1
        backingBean.requestHandler(blogBookmarkableId) >> Stub(BlogPostsPageRequestHandler) {
            iterator(_, _) >> [].iterator()
            size() >> 0
            getBlogId() >> blogId
        }
        backingBean.findBlogDisplayDetails(blogId) >> Option.of(blogDisplayDetails)

        when:
        tester.startPage(BlogPostsPage, new PageParameters().add(BLOG_BOOKMARKABLE_ID_PARAM, blogBookmarkableId))

        then:
        tester.assertComponent(BLOG_LINK, ExternalLink)
        tester.assertComponent(pathVia(DATA_VIEW_WRAPPER_ID, INFINITE_SCROLL_ID), InfinitePaginationPanel)
        tester.assertComponent(pathVia(DATA_VIEW_WRAPPER_ID, INFINITE_SCROLL_ID), InfinitePaginationPanel)
    }

    def "Should render list items"() {
        given:
        String blogBookmarkableId = "blogBookmarkableId"
        Long blogId = 10
        BlogDisplayDetails blogDisplayDetails = new BlogDisplayDetails("author", PERSONAL, "url")
        List<BlogPostForListing> blogPosts = [
                new BlogPostForListing(randomUUID().toString(), "title1", nowProvider.now()),
                new BlogPostForListing(randomUUID().toString(), "title2", nowProvider.now().minusDays(1)),
                new BlogPostForListing(randomUUID().toString(), "title3", nowProvider.now().minusDays(2))
        ]
        backingBean.defaultPageSize() >> 5
        backingBean.requestHandler(blogBookmarkableId) >> GroovyMock(BlogPostsPageRequestHandler) {
            iterator(_, _) >> blogPosts.iterator()
            size() >> blogPosts.size()
            getBlogId() >> blogId
            model(_) >>> [Model.of(blogPosts[0]), Model.of(blogPosts[1]), Model.of(blogPosts[2])]
        }
        backingBean.findBlogDisplayDetails(blogId) >> Option.of(blogDisplayDetails)
        backingBean.generateRedirectLink(_) >>> ["#1", "#2", "#3"]

        when:
        tester.startPage(BlogPostsPage, new PageParameters().add(BLOG_BOOKMARKABLE_ID_PARAM, blogBookmarkableId))

        then:
        tester.getComponentFromLastRenderedPage(pathVia(DATA_VIEW_WRAPPER_ID, DATA_VIEW_ID))
        Component component = tester.getComponentFromLastRenderedPage(pathVia(DATA_VIEW_WRAPPER_ID, DATA_VIEW_ID))
        (component as DataView).size() == 3
        tester.assertComponent(pathVia(DATA_VIEW_WRAPPER_ID, DATA_VIEW_ID, 1, LINK_ID), ExternalLink)
        tester.getTagByWicketId(LINK_ID).getAttributeContains("href", "#1")
        tester.assertComponent(pathVia(DATA_VIEW_WRAPPER_ID, DATA_VIEW_ID, 1, PUBLISHED_DATE_ID), Label)
        tester.getTagByWicketId(PUBLISHED_DATE_ID).value == blogPosts[0].publishedDate.format(DATE_FORMATTER)
    }

    def "Should render page header"() {
        given:
        String blogBookmarkableId = "blogBookmarkableId"
        Long blogId = 10
        BlogDisplayDetails blogDisplayDetails = new BlogDisplayDetails("author", COMPANY, "url")
        backingBean.defaultPageSize() >> 1
        backingBean.requestHandler(blogBookmarkableId) >> Stub(BlogPostsPageRequestHandler) {
            iterator(_, _) >> [].iterator()
            size() >> 0
            getBlogId() >> blogId
        }
        backingBean.findBlogDisplayDetails(blogId) >> Option.of(blogDisplayDetails)

        when:
        tester.startPage(BlogPostsPage, new PageParameters().add(BLOG_BOOKMARKABLE_ID_PARAM, blogBookmarkableId))

        then:
        tester.getTagByWicketId(BLOG_LINK).value == blogDisplayDetails.author
        tester.getTagByWicketId(BLOG_LINK).getAttributeContains("href", blogDisplayDetails.url)
        tester.getTagByWicketId(BACK_LINK).value.contains("Wróć do listy blogów")
        tester.assertBookmarkablePageLink(BACK_LINK, CompanyBlogsPage, new PageParameters())
    }

    def "Should point back to personal blogs if no blog details were found"() {
        given:
        String blogCode = "blog-code"
        Long blogId = 10
        backingBean.defaultPageSize() >> 1
        backingBean.requestHandler(blogCode) >> Stub(BlogPostsPageRequestHandler) {
            iterator(_, _) >> [].iterator()
            size() >> 0
            getBlogId() >> blogId
        }
        backingBean.findBlogDisplayDetails(blogId) >> Option.none()

        when:
        tester.startPage(BlogPostsPage, new PageParameters().add(BLOG_BOOKMARKABLE_ID_PARAM, blogCode))
        tester.clickLink("$BACK_LINK")

        then:
        tester.assertRenderedPage(PersonalBlogsPage)
    }

    def "Should generate next-link for infinity scroll"() {
        given:
        String blogBookmarkableId = "blogBookmarkableId"
        Long blogId = 10
        backingBean.defaultPageSize() >> 1
        backingBean.requestHandler(blogBookmarkableId) >> Stub(BlogPostsPageRequestHandler) {
            iterator(_, _) >> [].iterator()
            size() >> 0
            getBlogId() >> blogId
        }
        backingBean.findBlogDisplayDetails(blogId) >> Option.none()

        when:
        tester.startPage(BlogPostsPage, new PageParameters().add(BLOG_BOOKMARKABLE_ID_PARAM, blogBookmarkableId))

        then:
        tester.getTagByWicketId("next-page").getAttributeContains("href", "BlogPostsPage?-1.0")
    }
}
