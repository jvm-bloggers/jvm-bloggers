package com.jvm_bloggers.frontend.public_area.blogs

import com.jvm_bloggers.frontend.common_components.infinite_scroll.InfinitePaginationPanel
import com.jvm_bloggers.frontend.public_area.blogs.navigation.NavigationTabItem
import javaslang.collection.List
import org.apache.wicket.Component
import org.apache.wicket.markup.html.basic.Label
import org.apache.wicket.markup.html.link.BookmarkablePageLink
import org.apache.wicket.markup.html.link.ExternalLink
import org.apache.wicket.markup.repeater.data.DataView
import org.apache.wicket.request.mapper.parameter.PageParameters
import org.apache.wicket.util.tester.TagTester

import static com.jvm_bloggers.frontend.public_area.blogs.AbstractBlogsPage.*
import static com.jvm_bloggers.frontend.public_area.blogs.BlogWithStatisticsItemPopulator.AUTHOR_BLOG_LINK_ID
import static com.jvm_bloggers.frontend.public_area.blogs.BlogWithStatisticsItemPopulator.AUTHOR_ID
import static com.jvm_bloggers.frontend.public_area.blogs.BlogWithStatisticsItemPopulator.BLOG_POSTS_LINK_ID
import static com.jvm_bloggers.frontend.public_area.blogs.BlogWithStatisticsItemPopulator.FIRST_COUNTER_ID
import static com.jvm_bloggers.frontend.public_area.blogs.BlogWithStatisticsItemPopulator.SECOND_COUNTER_ID
import static com.jvm_bloggers.frontend.public_area.blogs.BlogWithStatisticsItemPopulator.URL_ID
import static com.jvm_bloggers.frontend.public_area.blogs.navigation.NavigationTabItem.ACTIVE_CSS_CLASS

class BlogsPageSpec extends BlogsPageSpecBase {

    def "Should render page with company blogs"() {
        when:
        tester.startPage(CompanyBlogsPage)

        then:
        tester.assertComponent(COMPANY_TAB_ID, NavigationTabItem)
        tester.assertBookmarkablePageLink("$COMPANY_TAB_ID:link", CompanyBlogsPage, new PageParameters())
        tester.assertComponent("$DATA_VIEW_WRAPPER_ID:$DATA_VIEW_ID", DataView)
        tester.assertComponent("$DATA_VIEW_WRAPPER_ID:$INFINITE_SCROLL_ID", InfinitePaginationPanel)
    }

    def "Should render page with personal blogs"() {
        when:
        tester.startPage(PersonalBlogsPage)

        then:
        tester.assertComponent(PERSONAL_TAB_ID, NavigationTabItem)
        tester.assertBookmarkablePageLink("$PERSONAL_TAB_ID:link", PersonalBlogsPage, new PageParameters())
        tester.assertComponent("$DATA_VIEW_WRAPPER_ID:$DATA_VIEW_ID", DataView)
        tester.assertComponent("$DATA_VIEW_WRAPPER_ID:$INFINITE_SCROLL_ID", InfinitePaginationPanel)
    }

    def "Should render page with video blogs"() {
        when:
        tester.startPage(VideoBlogsPage)

        then:
        tester.assertComponent(VIDEO_TAB_ID, NavigationTabItem)
        tester.assertBookmarkablePageLink("$VIDEO_TAB_ID:link", VideoBlogsPage, new PageParameters())
        tester.assertComponent("$DATA_VIEW_WRAPPER_ID:$DATA_VIEW_ID", DataView)
        tester.assertComponent("$DATA_VIEW_WRAPPER_ID:$INFINITE_SCROLL_ID", InfinitePaginationPanel)
    }

    def "Should set company tab as active"() {
        when:
        tester.startPage(CompanyBlogsPage)

        then:
        TagTester tagTester = tester.getTagByWicketId(COMPANY_TAB_ID)
        tagTester.getAttribute("class").contains(ACTIVE_CSS_CLASS)
    }

    def "Should set personal tab as active"() {
        when:
        tester.startPage(PersonalBlogsPage)

        then:
        TagTester tagTester = tester.getTagByWicketId(PERSONAL_TAB_ID)
        tagTester.getAttribute("class").contains(ACTIVE_CSS_CLASS)
    }

    def "Should set video tab as active"() {
        when:
        tester.startPage(VideoBlogsPage)

        then:
        TagTester tagTester = tester.getTagByWicketId(VIDEO_TAB_ID)
        tagTester.getAttribute("class").contains(ACTIVE_CSS_CLASS)
    }

    def "Should render grouped list of blogs"() {
        given:
        List blogStatistics = List.of(
                createBlogStatisticsForListing(1, 10, 20),
                createBlogStatisticsForListing(2, 10, 20),
                createBlogStatisticsForListing(3, 10, 20))
        blogRepository.countByBlogType(_) >> blogStatistics.length()
        blogStatisticsForListingQuery.findBlogPostStatistics(_, _) >> blogStatistics

        when:
        tester.startPage(PersonalBlogsPage)

        then:
        Component component = tester.getComponentFromLastRenderedPage("$DATA_VIEW_WRAPPER_ID:$DATA_VIEW_ID")
        (component as DataView).size() == 2
        tester.assertComponent("$DATA_VIEW_WRAPPER_ID:$DATA_VIEW_ID:1:$URL_ID", ExternalLink)
        tester.assertComponent("$DATA_VIEW_WRAPPER_ID:$DATA_VIEW_ID:1:$BLOG_POSTS_LINK_ID", BookmarkablePageLink)
        tester.assertComponent("$DATA_VIEW_WRAPPER_ID:$DATA_VIEW_ID:1:$AUTHOR_BLOG_LINK_ID", ExternalLink)
        tester.assertInvisible("$DATA_VIEW_WRAPPER_ID:$DATA_VIEW_ID:1:$AUTHOR_ID")
        tester.assertComponent("$DATA_VIEW_WRAPPER_ID:$DATA_VIEW_ID:1:$FIRST_COUNTER_ID", Label)
        tester.assertComponent("$DATA_VIEW_WRAPPER_ID:$DATA_VIEW_ID:1:$SECOND_COUNTER_ID", Label)
    }

    def "Should navigate to BlogPostsPage"() {
        given:
        List blogStatistics = List.of(createBlogStatisticsForListing(1, 10, 20))
        blogRepository.countByBlogType(_) >> blogStatistics.length()
        blogStatisticsForListingQuery.findBlogPostStatistics(_, _) >> blogStatistics

        when:
        tester.startPage(PersonalBlogsPage)
        tester.clickLink("$DATA_VIEW_WRAPPER_ID:$DATA_VIEW_ID:1:$BLOG_POSTS_LINK_ID")

        then:
        tester.assertRenderedPage(BlogPostsPage)
    }

    def "Should generate next-link for infinity scroll"() {
        given:
        List blogStatistics = List.of(
                createBlogStatisticsForListing(1, 10, 20),
                createBlogStatisticsForListing(2, 10, 20),
                createBlogStatisticsForListing(3, 10, 20))
        blogRepository.countByBlogType(_) >> blogStatistics.length()
        blogStatisticsForListingQuery.findBlogPostStatistics(_, _) >> blogStatistics

        when:
        tester.startPage(PersonalBlogsPage)

        then:
        tester.getTagByWicketId("next-page").getAttributeContains("href", "PersonalBlogsPage?0-1")
    }
}
