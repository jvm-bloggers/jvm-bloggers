package com.jvm_bloggers.frontend.admin_area.bot_clicks_detector

import com.jvm_bloggers.MockSpringContextAwareSpecification
import com.jvm_bloggers.entities.click.PostClicksCountByIpAddress
import com.jvm_bloggers.frontend.admin_area.PaginationConfiguration
import com.jvm_bloggers.frontend.admin_area.bot_clicks_detector.BotClicksDetectorPage.AggregatorType as AggregatorType

import io.vavr.collection.List

import org.apache.wicket.markup.html.form.DropDownChoice
import org.apache.wicket.markup.html.list.ListView

import static com.jvm_bloggers.frontend.WicketTestUtils.pathVia
import static com.jvm_bloggers.frontend.admin_area.bot_clicks_detector.BotClicksDetectorPage.AGGREGATOR_DROP_DOWN_ID
import static com.jvm_bloggers.frontend.admin_area.bot_clicks_detector.BotClicksDetectorPage.LIST_WRAPPER_ID
import static com.jvm_bloggers.frontend.admin_area.bot_clicks_detector.BotClicksDetectorPage.TOP_CLICKS_LIST_VIEW_ID

class BotClicksDetectorPageSpec extends MockSpringContextAwareSpecification {

    BotClicksDetectorPageBackingBean backingBean = Stub(BotClicksDetectorPageBackingBean)
    PaginationConfiguration paginationConfiguration = new PaginationConfiguration(15)

    @Override
    protected void setupContext() {
        addBean(backingBean)
        addBean(paginationConfiguration)
    }

    def "Should render BotClicksDetectorPage"() {
        when:
        tester.startPage(BotClicksDetectorPage)

        then:
        tester.assertRenderedPage(BotClicksDetectorPage)
        tester.assertContains("Bot clicks detector")
    }

    def "Should populate dropdown with correct values"() {
        when:
        tester.startPage(BotClicksDetectorPage)

        then:
        tester.assertComponent(AGGREGATOR_DROP_DOWN_ID, DropDownChoice)

        DropDownChoice<AggregatorType> dropDownChoice =
                tester.getComponentFromLastRenderedPage(AGGREGATOR_DROP_DOWN_ID) as DropDownChoice<AggregatorType>
        dropDownChoice.choices.size() == 2
        dropDownChoice.choices[0] == AggregatorType.IP_ADDRESS
        dropDownChoice.choices[1] == AggregatorType.USER_AGENT
    }

    def "Should render post clicks list"() {
        given:
        List<PostClicksCountByIpAddress> postClicksList = List.of(
                new PostClicksCountByIpAddress('title 1', 'author 1', '127.0.0.1', 100L),
                new PostClicksCountByIpAddress('title 2', 'author 2', '127.0.0.1', 50L)
        )
        backingBean.getTop10PostsClicksFromSameIpForPreviousMonth() >> postClicksList

        when:
        tester.startPage(BotClicksDetectorPage)

        then:
        tester.assertComponent(pathVia(LIST_WRAPPER_ID, TOP_CLICKS_LIST_VIEW_ID), ListView)
        tester.assertListView(pathVia(LIST_WRAPPER_ID, TOP_CLICKS_LIST_VIEW_ID), postClicksList.toJavaList())
    }
}
