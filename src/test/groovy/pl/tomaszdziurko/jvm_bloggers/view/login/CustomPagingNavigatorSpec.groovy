package pl.tomaszdziurko.jvm_bloggers.view.login

import org.apache.wicket.Component
import org.apache.wicket.markup.repeater.data.DataView
import org.apache.wicket.model.Model
import org.apache.wicket.protocol.http.WebApplication
import org.apache.wicket.util.tester.WicketTester
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.test.util.ReflectionTestUtils
import pl.tomaszdziurko.jvm_bloggers.SpringContextAwareSpecification
import pl.tomaszdziurko.jvm_bloggers.view.login.helpers.TestDataNavigationPage

class CustomPagingNavigatorSpec extends SpringContextAwareSpecification {

    private WicketTester tester

    DataView dataView = Mock(DataView)

    @Autowired
    private WebApplication wicketApplication

    def setup() {
        // This is a workaround for problems with Spring Boot and WicketTester
        // https://issues.apache.org/jira/browse/WICKET-6053 and
        // https://github.com/MarcGiffing/wicket-spring-boot/issues/31
        ReflectionTestUtils.setField(wicketApplication, "name", null)
        tester = new WicketTester(wicketApplication)
    }

    def "Should contain label with total numbers of pages"() {
        when:
            tester.startPage(new TestDataNavigationPage(10, 100))
        then:
            tester.assertVisible("navigator:total")
            tester.assertLabel("navigator:total", "Total pages: 10")
    }

    def "Should navigate current page active"() {
        when:
            tester.startPage(new TestDataNavigationPage(10, 100))
        then:
            Component component = tester.getComponentFromLastRenderedPage("navigator:navigation:0")s

            tester.assertModelValue("navigator:navigation:0", Model.of("active"))
    }

}
