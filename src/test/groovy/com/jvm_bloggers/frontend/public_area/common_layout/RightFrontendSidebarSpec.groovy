package com.jvm_bloggers.frontend.public_area.common_layout

import com.jvm_bloggers.MockSpringContextAwareSpecification
import com.jvm_bloggers.frontend.public_area.varia_suggestion.VariaSuggestionPage
import com.jvm_bloggers.frontend.public_area.varia_suggestion.VariaSuggestionPageBackingBean

class RightFrontendSidebarSpec extends MockSpringContextAwareSpecification {

    @Override
    protected void setupContext() {
        addBean(Stub(RightFrontendSidebarBackingBean))
        addBean(Stub(VariaSuggestionPageBackingBean))
    }

    def "Should navigate to varia suggestion page"() {
        when:
        tester.startComponentInPage(RightFrontendSidebar)
        tester.clickLink('variaSuggestionLink')

        then:
        tester.assertRenderedPage(VariaSuggestionPage)
    }

}
