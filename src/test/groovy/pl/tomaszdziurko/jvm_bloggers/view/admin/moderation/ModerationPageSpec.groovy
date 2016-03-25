package pl.tomaszdziurko.jvm_bloggers.view.admin.moderation

import org.apache.wicket.protocol.http.WebApplication
import org.apache.wicket.util.tester.FormTester
import org.apache.wicket.util.tester.WicketTester
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.test.util.ReflectionTestUtils
import pl.tomaszdziurko.jvm_bloggers.SpringContextAwareSpecification
import pl.tomaszdziurko.jvm_bloggers.blog_posts.DummyBlogPostsFetcher
import pl.tomaszdziurko.jvm_bloggers.blogs.DummyBloggersDataFetcher
import pl.tomaszdziurko.jvm_bloggers.view.admin.AdminDashboardPage
import pl.tomaszdziurko.jvm_bloggers.view.login.LoginPage

class ModerationPageSpec extends SpringContextAwareSpecification {

    private WicketTester tester

    @Autowired
    WebApplication wicketApplication

    @Autowired
    DummyBloggersDataFetcher dummyBloggersDataFetcher;

    @Autowired
    DummyBlogPostsFetcher dummyBlogPostsFetcher;

    def setup() {
        ReflectionTestUtils.setField(wicketApplication, "name", null)
        tester = new WicketTester(wicketApplication)
    }

    def "Should fetch new blogs"() {
        when:
            navigateToModerationPageAsAdmin()
            tester.assertRenderedPage(ModerationPage.class)
            tester.clickLink(ModerationPage.FETCH_NEW_BLOGS_BUTTON_ID);
        then:
            validateModerationPage()
            dummyBloggersDataFetcher.isRefreshed()
    }

    def "Should fetch new posts"() {
        when:
            navigateToModerationPageAsAdmin()
            tester.assertRenderedPage(ModerationPage.class)
            tester.clickLink(ModerationPage.FETCH_NEW_POSTS_BUTTON_ID);
        then:
            validateModerationPage()
            dummyBlogPostsFetcher.isRefreshed()
    }

    void validateModerationPage() {
        tester.assertNoErrorMessage()
        tester.assertRenderedPage(ModerationPage.class)
    }

    void navigateToModerationPageAsAdmin() {
        tester.startPage(LoginPage.class)
        FormTester formTester = tester.newFormTester(LoginPage.LOGIN_FORM_ID)
        formTester.setValue(LoginPage.LOGIN_FIELD_ID, "admin")
        formTester.setValue(LoginPage.PASSWORD_FIELD_ID, PASSWORD)
        formTester.submit(LoginPage.FORM_SUBMIT_ID)

        tester.assertRenderedPage(AdminDashboardPage.class)
        tester.clickLink("moderationDetailsLink");
    }
}