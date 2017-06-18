package com.jvm_bloggers.frontend.public_area.blogs.timeline_panel

import com.jvm_bloggers.MockSpringContextAwareSpecification

import java.time.Month
import java.time.YearMonth

class YearMonthSeparatorPanelSpec extends MockSpringContextAwareSpecification {

    def "Should render panel"() {
        given:
        YearMonth yearMonth = YearMonth.of(2017, Month.JANUARY)
        YearMonthSeparatorPanel yearMonthSeparatorPanel = new YearMonthSeparatorPanel("id", yearMonth)

        when:
        tester.startComponentInPage(yearMonthSeparatorPanel)

        then:
        tester.assertLabel("id:monthYear", "styczeń 2017")
    }

    @Override
    protected void setupContext() {}
}
