package pl.tomaszdziurko.jvm_bloggers.view.login;

import lombok.extern.slf4j.Slf4j;

import org.apache.wicket.RestartResponseAtInterceptPageException;
import org.apache.wicket.authroles.authorization.strategies.role.Roles;
import org.apache.wicket.devutils.stateless.StatelessComponent;
import org.apache.wicket.markup.html.WebPage;
import org.apache.wicket.markup.html.form.Button;
import org.apache.wicket.markup.html.form.PasswordTextField;
import org.apache.wicket.markup.html.form.RequiredTextField;
import org.apache.wicket.markup.html.form.StatelessForm;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.protocol.http.servlet.ServletWebRequest;
import org.apache.wicket.spring.injection.annot.SpringBean;

import pl.tomaszdziurko.jvm_bloggers.view.admin.AdminDashboardPage;
import pl.tomaszdziurko.jvm_bloggers.view.login.attack.BruteForceAttackEvent;
import pl.tomaszdziurko.jvm_bloggers.view.login.attack.BruteForceLoginAttackDetector;
import pl.tomaszdziurko.jvm_bloggers.view.login.attack.stream.BruteForceAttackEventStreamManager;
import pl.tomaszdziurko.jvm_bloggers.view.panels.CustomFeedbackPanel;
import pl.tomaszdziurko.jvm_bloggers.view.session.UserSession;

import javax.servlet.http.HttpServletRequest;

@StatelessComponent
public class LoginPage extends WebPage {

    public static final String LOGIN_FORM_ID = "loginForm";
    public static final String LOGIN_FIELD_ID = "login";
    public static final String PASSWORD_FIELD_ID = "password";
    public static final String FORM_SUBMIT_ID = "submit";

    private String login;
    private String password;

    @SpringBean
    private UserAuthenticator userAuthenticator;

    @SpringBean
    private BruteForceLoginAttackDetector bruteForceLoginAttackDetector;

    @SpringBean
    private BruteForceAttackEventStreamManager bruteForceAttackEventStreamManager;

    public LoginPage() {
        StatelessForm<LoginPage> loginForm = new StatelessForm<LoginPage>(LOGIN_FORM_ID,
            new CompoundPropertyModel<>(this)) {
            @Override
            protected void onSubmit() {
                final String clientAddress = getClientAddress();
                final boolean
                    bruteForceAttackDetected =
                    bruteForceLoginAttackDetector.isItBruteForceAttack(clientAddress);
                if (bruteForceAttackDetected) {
                    handleBruteForceAttack(clientAddress);
                    return;
                }
                tryToLoginUser(clientAddress);
            }

            private void tryToLoginUser(String clientAddress) {
                final Roles roles = userAuthenticator.getRolesForUser(login, password);
                if (roles.hasRole(Roles.ADMIN)) {
                    UserSession.get().loginAs(login, roles);
                    if (RestartResponseAtInterceptPageException.getOriginalUrl() != null) {
                        continueToOriginalDestination();
                    } else {
                        setResponsePage(AdminDashboardPage.class);
                    }
                } else {
                    bruteForceLoginAttackDetector.recordInvalidLoginAttempt(clientAddress);
                    error("Incorrect login or password");
                }
            }
        };

        CustomFeedbackPanel feedbackPanel = new CustomFeedbackPanel("feedbackPanel");
        loginForm.add(feedbackPanel);
        RequiredTextField<String> loginField = new RequiredTextField<>(LOGIN_FIELD_ID);
        loginForm.add(loginField);
        PasswordTextField passwordField = new PasswordTextField(PASSWORD_FIELD_ID);
        loginForm.add(passwordField);
        Button loginButton = new Button(FORM_SUBMIT_ID);
        loginForm.add(loginButton);
        add(loginForm);
    }

    private void handleBruteForceAttack(final String clientAddress) {
        error("Incorrect login or password [BruteForce attack was detected]");
        bruteForceAttackEventStreamManager.createEventStreamFor(clientAddress)
            .publish(BruteForceAttackEvent.builder().ipAddress(clientAddress).build());
    }

    private String getClientAddress() {
        ServletWebRequest servletWebRequest = (ServletWebRequest) getRequest();
        HttpServletRequest request = servletWebRequest.getContainerRequest();
        return request.getRemoteAddr();
    }
}
