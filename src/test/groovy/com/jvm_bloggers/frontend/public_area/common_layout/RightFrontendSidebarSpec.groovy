package com.jvm_bloggers.frontend.public_area.common_layout

import com.jvm_bloggers.MockSpringContextAwareSpecification
import com.jvm_bloggers.frontend.public_area.jvm_poland_slack.JvmPolandSlackPage
import com.jvm_bloggers.frontend.public_area.jvm_poland_slack.JvmPolandSlackPageBackingBean
import com.jvm_bloggers.frontend.public_area.varia_suggestion.VariaSuggestionPage
import com.jvm_bloggers.frontend.public_area.varia_suggestion.VariaSuggestionPageBackingBean

class RightFrontendSidebarSpec extends MockSpringContextAwareSpecification {

    @Override
    protected void setupContext() {
        addBean(Stub(RightFrontendSidebarBackingBean))
        addBean(Stub(VariaSuggestionPageBackingBean))
        addBean(Stub(JvmPolandSlackPageBackingBean))
    }

    def "Should navigate to varia suggestion page"() {
        when:
        tester.startComponentInPage(RightFrontendSidebar)
        tester.clickLink('variaSuggestionLink')

        then:
        tester.assertRenderedPage(VariaSuggestionPage)
    }

    def "Should navigate to jvm poland slack page"() {
        when:
        tester.startComponentInPage(RightFrontendSidebar)
        tester.clickLink('jvm-poland-slack')

        then:
        tester.assertRenderedPage(JvmPolandSlackPage)
    }

}
