package com.jvm_bloggers.frontend.public_area.rss

import com.jvm_bloggers.MockSpringContextAwareSpecification
import com.jvm_bloggers.frontend.public_area.common_layout.RightFrontendSidebarBackingBean
import org.apache.wicket.markup.html.link.ExternalLink
import spock.lang.Unroll

class RssInformationPageSpec extends MockSpringContextAwareSpecification {

    RightFrontendSidebarBackingBean sidebarBean = Stub(RightFrontendSidebarBackingBean)

    @Override
    protected void setupContext() {
        addBean(sidebarBean)
    }

    @Unroll
    def "Should contain #element element"() {
    when:
        tester.startPage(RssInformationPage)

    then:
        tester.assertComponent(element.id, ExternalLink)

    where:
        element << RssInformationPage.FeedUrl.values()
    }
}
