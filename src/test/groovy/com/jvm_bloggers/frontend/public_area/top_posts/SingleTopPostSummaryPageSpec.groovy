package com.jvm_bloggers.frontend.public_area.top_posts

import com.jvm_bloggers.MockSpringContextAwareSpecification
import com.jvm_bloggers.domain.query.published_newsletter_issue.PublishedPost
import com.jvm_bloggers.domain.query.top_posts_summary.PublishedTopPostSummary
import com.jvm_bloggers.frontend.public_area.common_layout.RightFrontendSidebarBackingBean
import com.jvm_bloggers.utils.DateTimeUtilities
import io.vavr.collection.List as VavrList
import io.vavr.control.Option

import java.time.YearMonth

import static com.jvm_bloggers.frontend.WicketTestUtils.pathVia
import static com.jvm_bloggers.frontend.public_area.top_posts.SingleTopPostSummaryPage.*

class SingleTopPostSummaryPageSpec extends MockSpringContextAwareSpecification {

    SingleTopPostSummaryPageBackingBean backingBean = Stub(SingleTopPostSummaryPageBackingBean)

    @Override
    protected void setupContext() {
        addBean(backingBean)
        addBean(Stub(RightFrontendSidebarBackingBean))
        addBean(Stub(TopPostsPageBackingBean))
    }

    def "should redirect on missing parameters"() {
        when:
        tester.startPage(SingleTopPostSummaryPage)

        then:
        tester.assertRenderedPage(TopPostsPage)
    }

    def "should redirect when summary does not exist"() {
        given:
        YearMonth yearMonth = YearMonth.now()
        backingBean.findSummaryFor(yearMonth) >> Option.none()

        when:
        tester.startPage(SingleTopPostSummaryPage, buildPageParams(yearMonth))

        then:
        tester.assertRenderedPage(TopPostsPage)
    }

    def "should display summary"() {
        given:
        YearMonth yearMonth = YearMonth.now()
        backingBean.findSummaryFor(yearMonth) >> Option.of(createSummary(yearMonth))

        when:
        tester.startPage(SingleTopPostSummaryPage, buildPageParams(yearMonth))

        then:
        tester.assertRenderedPage(SingleTopPostSummaryPage)
        tester.assertContains(DateTimeUtilities.stringify(yearMonth) + " - Najlepsze posty")
        tester.assertVisible(pathVia(TOP_PERSONAL_POSTS_ID, "0", BLOG_POST_LINK_ID))
        tester.assertVisible(pathVia(TOP_PERSONAL_POSTS_ID, "1", BLOG_POST_LINK_ID))
        tester.assertVisible(pathVia(TOP_PERSONAL_POSTS_ID, "2", BLOG_POST_LINK_ID))

        tester.assertVisible(pathVia(TOP_COMPANY_POSTS_ID, "0", BLOG_POST_LINK_ID))
        tester.assertVisible(pathVia(TOP_COMPANY_POSTS_ID, "1", BLOG_POST_LINK_ID))
    }

    private PublishedTopPostSummary createSummary(YearMonth yearMonth) {
        return new PublishedTopPostSummary(yearMonth,
                VavrList.of(createPost("Title 1"), createPost("Title 2"), createPost("Title 3")),
                VavrList.of(createPost("Title A"), createPost("Title B"))
        )
    }

    private  PublishedPost createPost(String title) {
        return PublishedPost.builder()
        .authorName("Author of " + title)
        .title(title)
        .url("http://example.com")
        .build()
    }
}
