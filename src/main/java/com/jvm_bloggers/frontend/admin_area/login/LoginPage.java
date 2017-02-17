package com.jvm_bloggers.frontend.admin_area.login;

import com.jvm_bloggers.frontend.admin_area.AdminDashboardPage;
import com.jvm_bloggers.frontend.admin_area.login.attack.BruteForceLoginAttackDetector;
import com.jvm_bloggers.frontend.admin_area.panels.CustomFeedbackPanel;
import com.jvm_bloggers.frontend.admin_area.session.UserSession;

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
import org.wicketstuff.annotation.mount.MountPath;

import javax.servlet.http.HttpServletRequest;

@MountPath("login")
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
                    error("Incorrect login or password [BruteForce attack was detected]");
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

    private String getClientAddress() {
        ServletWebRequest servletWebRequest = (ServletWebRequest) getRequest();
        HttpServletRequest request = servletWebRequest.getContainerRequest();
        return request.getRemoteAddr();
    }
}
