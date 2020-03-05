package com.jvm_bloggers.frontend.admin_area.login

import com.jvm_bloggers.MockSpringContextAwareSpecification
import com.jvm_bloggers.entities.blog_post.BlogPostRepository
import com.jvm_bloggers.entities.top_posts_summary.TopPostsSummaryRepository
import com.jvm_bloggers.frontend.admin_area.AdminDashboardPage
import com.jvm_bloggers.frontend.admin_area.AdminDashboardPageBackingBean
import com.jvm_bloggers.frontend.admin_area.PaginationConfiguration
import com.jvm_bloggers.frontend.admin_area.login.attack.BruteForceLoginAttackDetector
import com.jvm_bloggers.utils.ZoneTimeProvider
import org.apache.wicket.authroles.authorization.strategies.role.Roles
import org.apache.wicket.util.tester.FormTester

class LoginPageSpec extends MockSpringContextAwareSpecification {

    UserAuthenticator userAuthenticator = Stub()

    Roles adminRoles = Stub() {
        hasRole(Roles.ADMIN) >> true
    }

    Roles notAdminRoles = Stub() {
        hasRole(Roles.ADMIN) >> false
    }

    void setupContext() {
        ZoneTimeProvider nowProvider = new ZoneTimeProvider()
        addBean(nowProvider)
        addBean(userAuthenticator)
        addBean(new BruteForceLoginAttackDetector())
        addBean(Mock(BlogPostRepository))
        addBean(new PaginationConfiguration(15))
        addBean(Stub(AdminDashboardPageBackingBean))
        addBean(Stub(TopPostsSummaryRepository))
    }

    def "Should redirect to Admin Dashboard after successful login"() {
        given:
        String login = "login"
        String pass = "pass"
        userAuthenticator.getRolesForUser(login, pass) >> adminRoles

        when:
        tester.startPage(LoginPage.class)
        FormTester formTester = tester.newFormTester(LoginPage.LOGIN_FORM_ID)
        formTester.setValue(LoginPage.LOGIN_FIELD_ID, login)
        formTester.setValue(LoginPage.PASSWORD_FIELD_ID, pass)
        formTester.submit(LoginPage.FORM_SUBMIT_ID)

        then:
        tester.assertNoErrorMessage()
        tester.assertRenderedPage(AdminDashboardPage.class)
    }

    def "Should reject incorrect password"() {
        given:
        userAuthenticator.getRolesForUser(_, _) >> notAdminRoles

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

    def "Should not try to login after brute force attack was detected"() {
        given:
        userAuthenticator.getRolesForUser(_, _) >> notAdminRoles

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
}