package com.jvm_bloggers.frontend.common_components

import com.jvm_bloggers.MockSpringContextAwareSpecification
import com.jvm_bloggers.frontend.common_components.infinite_scroll.InfinitePaginationPanel
import com.jvm_bloggers.frontend.common_components.infinite_scroll.InfiniteScrollingBehavior
import org.apache.wicket.markup.repeater.data.DataView

class InfinitePaginationPanelSpec extends MockSpringContextAwareSpecification {

    def "Should add InfiniteScrollingBehavior to component when pageCount > 1"() {
        given:
        String panelId = "someId"
        DataView dataView = Stub(DataView)
        dataView.getPageCount() >> 2
        InfinitePaginationPanel infinitePaginationPanel = new InfinitePaginationPanel(panelId, dataView)

        when:
        tester.startComponentInPage(infinitePaginationPanel)

        then:
        1 == tester.getComponentFromLastRenderedPage(panelId).getBehaviors(InfiniteScrollingBehavior).size()
    }

    def "Should don't add InfiniteScrollingBehavior to component when pageCount <= 1"() {
        given:
        String panelId = "someId"
        DataView dataView = Stub(DataView)
        dataView.getPageCount() >> 1
        InfinitePaginationPanel infinitePaginationPanel = new InfinitePaginationPanel(panelId, dataView)

        when:
        tester.startComponentInPage(infinitePaginationPanel)

        then:
        tester.getComponentFromLastRenderedPage(panelId).getBehaviors(InfiniteScrollingBehavior).isEmpty()
    }

    @Override
    protected void setupContext() {

    }
}
