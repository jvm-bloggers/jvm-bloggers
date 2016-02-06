package pl.tomaszdziurko.jvm_bloggers.view.login;

import lombok.extern.slf4j.Slf4j;
import org.apache.wicket.devutils.stateless.StatelessComponent;
import org.apache.wicket.markup.html.form.Button;
import org.apache.wicket.markup.html.form.PasswordTextField;
import org.apache.wicket.markup.html.form.RequiredTextField;
import org.apache.wicket.markup.html.form.StatelessForm;
import org.apache.wicket.model.CompoundPropertyModel;
import pl.tomaszdziurko.jvm_bloggers.view.BasePage;
import pl.tomaszdziurko.jvm_bloggers.view.panels.CustomFeedbackPanel;

@StatelessComponent
@Slf4j
public class LoginPage extends BasePage {

    private String login;
    private String password;

    public LoginPage() {
        StatelessForm<LoginPage> loginForm = new StatelessForm<LoginPage>("loginForm", new CompoundPropertyModel<>(this)) {
            @Override
            protected void onSubmit() {
                log.info("Logging using " + login + " and " + password);
                success("Success!");
            }
        };
        CustomFeedbackPanel feedbackPanel = new CustomFeedbackPanel("feedbackPanel");
        loginForm.add(feedbackPanel);
        RequiredTextField loginField = new RequiredTextField("login");
        loginForm.add(loginField);
        PasswordTextField passwordField = new PasswordTextField("password");
        loginForm.add(passwordField);
        Button loginButton = new Button("submit");
        loginForm.add(loginButton);
        add(loginForm);
    }
}
