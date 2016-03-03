package pl.tomaszdziurko.jvm_bloggers.view.login

import org.apache.wicket.protocol.http.WebApplication
import org.apache.wicket.util.tester.FormTester
import org.apache.wicket.util.tester.WicketTester
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.test.SpringApplicationContextLoader
import org.springframework.test.context.ContextConfiguration
import org.springframework.test.util.ReflectionTestUtils
import pl.tomaszdziurko.jvm_bloggers.JvmBloggersApplication
import pl.tomaszdziurko.jvm_bloggers.SpringContextAwareSpecification;
import pl.tomaszdziurko.jvm_bloggers.view.admin.AdminDashboardPage
import spock.lang.Specification


class LoginPageSpec extends SpringContextAwareSpecification {

    private WicketTester tester

    @Autowired
    private WebApplication wicketApplication

    def setup() {
        // This is a workaround for problems with Spring Boot and WicketTester
        // https://issues.apache.org/jira/browse/WICKET-6053 and
        // https://github.com/MarcGiffing/wicket-spring-boot/issues/31
        ReflectionTestUtils.setField(wicketApplication, "name", null)
        tester = new WicketTester(wicketApplication)
    }

    def "Should redirect to Admin Dashboard after successful login"() {
        when:
            tester.startPage(LoginPage.class)
            FormTester formTester = tester.newFormTester(LoginPage.LOGIN_FORM_ID)
            formTester.setValue(LoginPage.LOGIN_FIELD_ID, "Any User")
            formTester.setValue(LoginPage.PASSWORD_FIELD_ID, PASSWORD)
            formTester.submit(LoginPage.FORM_SUBMIT_ID)
        then:
            tester.assertNoErrorMessage()
            tester.assertRenderedPage(AdminDashboardPage.class)
    }

    def "Should reject incorrect password"() {
        when:
            tester.startPage(LoginPage.class)
            FormTester formTester = tester.newFormTester(LoginPage.LOGIN_FORM_ID)
            formTester.setValue(LoginPage.LOGIN_FIELD_ID, "Any User")
            formTester.setValue(LoginPage.PASSWORD_FIELD_ID, "incorrect password")
            formTester.submit(LoginPage.FORM_SUBMIT_ID)
        then:
            tester.assertErrorMessages("Incorrect login or password")
            tester.assertRenderedPage(LoginPage.class)
    }



}