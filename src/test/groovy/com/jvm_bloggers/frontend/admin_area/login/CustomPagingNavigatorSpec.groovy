package com.jvm_bloggers.frontend.admin_area.login

import com.jvm_bloggers.MockSpringContextAwareSpecification
import com.jvm_bloggers.frontend.admin_area.panels.CustomPagingNavigator
import org.apache.wicket.AttributeModifier
import org.apache.wicket.Component
import org.apache.wicket.markup.html.navigation.paging.IPageable
import org.apache.wicket.model.Model

class CustomPagingNavigatorSpec extends MockSpringContextAwareSpecification {

    def "Should render a label with total numbers of pages"() {
        given:
        int numberOfPages = 10
        IPageable pageable = Stub(IPageable)
        pageable.getPageCount() >> numberOfPages
        pageable.getCurrentPage() >> 3
        CustomPagingNavigator customPaging = new CustomPagingNavigator("someId", pageable)

        when:
        tester.startComponentInPage(customPaging)

        then:
        tester.assertVisible("someId:total")
        tester.assertLabel("someId:total", "Total pages: $numberOfPages")
    }

    def "Should mark button for current page as active"() {
        given:
        int currentPage = 3
        IPageable pageable = Stub(IPageable)
        pageable.getPageCount() >> 10
        pageable.getCurrentPage() >> currentPage
        CustomPagingNavigator customPaging = new CustomPagingNavigator("someId", pageable)

        when:
        tester.startComponentInPage(customPaging)

        then:
        Component activePageNumberItem = tester.getComponentFromLastRenderedPage("someId:navigation:$currentPage")
        activePageNumberItem.getBehaviors(AttributeModifier).any {
            it.attribute == "class" && it.getReplaceModel() == Model.of("active")
        }
    }

    @Override
    protected void setupContext() {

    }
}