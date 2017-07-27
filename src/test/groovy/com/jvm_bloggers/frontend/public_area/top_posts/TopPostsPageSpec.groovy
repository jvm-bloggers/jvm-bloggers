package com.jvm_bloggers.frontend.public_area.top_posts

import com.jvm_bloggers.MockSpringContextAwareSpecification
import com.jvm_bloggers.domain.query.top_posts_summary.TopPostsSummaryBasicDetails
import com.jvm_bloggers.frontend.public_area.common_layout.RightFrontendSidebarBackingBean
import io.vavr.collection.List as VavrList

import java.time.YearMonth

import static com.jvm_bloggers.frontend.WicketTestUtils.pathVia

class TopPostsPageSpec extends MockSpringContextAwareSpecification {

    TopPostsPageBackingBean backingBean = Stub(TopPostsPageBackingBean)

    @Override
    protected void setupContext() {
        addBean(backingBean)
        addBean(Stub(RightFrontendSidebarBackingBean))
    }

    def "should display two posts summaries"() {
        given:
        backingBean.getAllSummaries() >> VavrList.of(
            new TopPostsSummaryBasicDetails(YearMonth.now()),
            new TopPostsSummaryBasicDetails(YearMonth.now().minusMonths(1))
        )

        when:
        tester.startPage(TopPostsPage)

        then:
        tester.assertRenderedPage(TopPostsPage)
        tester.assertVisible(pathVia(TopPostsPage.TOP_POST_SUMMARIES_ID, "0", TopPostsPage.SUMMARY_LINK_ID))
        tester.assertVisible(pathVia(TopPostsPage.TOP_POST_SUMMARIES_ID, "1", TopPostsPage.SUMMARY_LINK_ID))
    }

}
