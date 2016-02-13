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
import org.apache.wicket.spring.injection.annot.SpringBean;
import pl.tomaszdziurko.jvm_bloggers.view.admin.AdminDashboardPage;
import pl.tomaszdziurko.jvm_bloggers.view.panels.CustomFeedbackPanel;
import pl.tomaszdziurko.jvm_bloggers.view.session.UserSession;

@StatelessComponent
@Slf4j
public class LoginPage extends WebPage {

    public static final String LOGIN_FORM_ID = "loginForm";
    public static final String LOGIN_FIELD_ID = "login";
    public static final String PASSWORD_FIELD_ID = "password";
    public static final String FORM_SUBMIT_ID = "submit";

    private String login;
    private String password;

    @SpringBean
    private UserAuthenticator userAuthenticator;

    public LoginPage() {
        StatelessForm<LoginPage> loginForm = new StatelessForm<LoginPage>(LOGIN_FORM_ID, new CompoundPropertyModel<>(this)) {
            @Override
            protected void onSubmit() {
                log.info("Login attempt as user " + login);
                Roles roles = userAuthenticator.getRolesForUser(login, password);

                if (roles.hasRole(Roles.ADMIN)) {
                    UserSession.get().loginAs(login, roles);
                    if (RestartResponseAtInterceptPageException.getOriginalUrl() != null) {
                        continueToOriginalDestination();
                    } else {
                        setResponsePage(AdminDashboardPage.class);
                    }
                } else {
                    log.warn("Invalid login credentials");
                    error("Incorrect login or password");
                }
            }
        };
        CustomFeedbackPanel feedbackPanel = new CustomFeedbackPanel("feedbackPanel");
        loginForm.add(feedbackPanel);
        RequiredTextField loginField = new RequiredTextField(LOGIN_FIELD_ID);
        loginForm.add(loginField);
        PasswordTextField passwordField = new PasswordTextField(PASSWORD_FIELD_ID);
        loginForm.add(passwordField);
        Button loginButton =  new Button(FORM_SUBMIT_ID);
        loginForm.add(loginButton);
        add(loginForm);
    }
}
