package com.jvm_bloggers.frontend.common_components.toastr

import com.jvm_bloggers.MockSpringContextAwareSpecification
import com.jvm_bloggers.frontend.public_area.common_layout.RightFrontendSidebarBackingBean
import com.jvm_bloggers.frontend.public_area.varia_suggestion.VariaSuggestionPage
import com.jvm_bloggers.frontend.public_area.varia_suggestion.VariaSuggestionPageBackingBean

class ToastrBehaviorSpec extends MockSpringContextAwareSpecification {

    @Override
    protected void setupContext() {
        addBean(Stub(RightFrontendSidebarBackingBean))
        addBean(Stub(VariaSuggestionPageBackingBean))
    }

//    def "Should add toast to page"() {
//        when:
//        tester.startPage(VariaSuggestionPage)
//        String responseAsString = tester.getLastResponseAsString()
//
//        then:
//        responseAsString.contains('toastr.min.css')
//        responseAsString.contains('toastr.min.js')
//    }
}
