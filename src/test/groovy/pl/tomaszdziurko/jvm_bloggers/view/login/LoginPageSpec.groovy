package pl.tomaszdziurko.jvm_bloggers.view.login

import org.apache.wicket.protocol.http.WebApplication
import org.apache.wicket.util.tester.FormTester
import org.apache.wicket.util.tester.WicketTester
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.test.util.ReflectionTestUtils
import pl.tomaszdziurko.jvm_bloggers.SpringContextAwareSpecification
import pl.tomaszdziurko.jvm_bloggers.mailing.LogMailPostAction
import pl.tomaszdziurko.jvm_bloggers.mailing.LogMailSender
import pl.tomaszdziurko.jvm_bloggers.view.admin.AdminDashboardPage
import pl.tomaszdziurko.jvm_bloggers.view.login.attack.stream.BruteForceAttackEventStreamManager
import rx.schedulers.TestScheduler

import java.lang.Void as Should
import java.util.concurrent.TimeUnit

import static BruteForceAttackEventStreamManager.MAILING_TIME_THROTTLE_IN_MINUTES

class LoginPageSpec extends SpringContextAwareSpecification {

    private WicketTester tester

    @Autowired
    private WebApplication wicketApplication

    @Autowired
    private LogMailSender logMailSender;

    @Autowired
    private TestScheduler scheduler;

    @Autowired
    private LogMailPostAction logMailPostAction;

    def setup() {
        // This is a workaround for problems with Spring Boot and WicketTester
        // https://issues.apache.org/jira/browse/WICKET-6053 and
        // https://github.com/MarcGiffing/wicket-spring-boot/issues/31
        ReflectionTestUtils.setField(wicketApplication, "name", null)
        tester = new WicketTester(wicketApplication)
    }

    Should "Redirect to Admin Dashboard after successful login"() {
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

    Should "Reject incorrect password"() {
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

    Should "Not try to login after brute force attack was detected"() {
        when:
        (1..4).each {
            tester.startPage(LoginPage.class)
            FormTester formTester = tester.newFormTester(LoginPage.LOGIN_FORM_ID)
            formTester.setValue(LoginPage.LOGIN_FIELD_ID, "Any User")
            formTester.setValue(LoginPage.PASSWORD_FIELD_ID, "incorrect password")
            formTester.submit(LoginPage.FORM_SUBMIT_ID)
        }

        then:
        tester.assertErrorMessages("Incorrect login or password [BruteForce attack was detected]")
        tester.assertRenderedPage(LoginPage.class)
    }

    Should "Not send e-mail more than twice after mutiple brute force attacka were detected"() {
        given:
        logMailPostAction.init(2)

        when:
        (1..10).each {
            tester.startPage(LoginPage.class)
            FormTester formTester = tester.newFormTester(LoginPage.LOGIN_FORM_ID)
            formTester.setValue(LoginPage.LOGIN_FIELD_ID, "Any User")
            formTester.setValue(LoginPage.PASSWORD_FIELD_ID, "incorrect password")
            formTester.submit(LoginPage.FORM_SUBMIT_ID)

            if (it.intValue() == 5) {
                scheduler.advanceTimeTo(MAILING_TIME_THROTTLE_IN_MINUTES, TimeUnit.MINUTES);
            } else if (it.intValue() == 10) {
                scheduler.advanceTimeTo(MAILING_TIME_THROTTLE_IN_MINUTES * 2, TimeUnit.MINUTES);
            }
        }

        then:
        tester.assertErrorMessages("Incorrect login or password [BruteForce attack was detected]")
        tester.assertRenderedPage(LoginPage.class)
        logMailSender.getMailPostAction().awaitAction();
        scheduler.triggerActions()
    }
}