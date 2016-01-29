package pl.tomaszdziurko.jvm_bloggers.view.login;

import org.apache.wicket.devutils.stateless.StatelessComponent;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.model.ComponentModel;
import pl.tomaszdziurko.jvm_bloggers.view.BasePage;

@StatelessComponent
public class LoginPage extends BasePage {

    private String login;
    private String password;

    public LoginPage() {

        Form<Void> loginForm = new Form<Void>("loginForm", new ComponentModel<LoginPage>());
    }
}
