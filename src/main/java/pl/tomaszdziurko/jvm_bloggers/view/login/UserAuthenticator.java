package pl.tomaszdziurko.jvm_bloggers.view.login;


import org.apache.wicket.authroles.authorization.strategies.role.Roles;
import org.springframework.stereotype.Service;

@Service
public class UserAuthenticator {

    public Roles getRolesForUser(String login, String password) {
        String jasyptPassword = System.getProperty("jasypt.encryptor.password");

        if (jasyptPassword.equals(password)) {
            return new Roles(Roles.ADMIN);
        } else {
            return new Roles(Roles.USER);
        }
    }
}
