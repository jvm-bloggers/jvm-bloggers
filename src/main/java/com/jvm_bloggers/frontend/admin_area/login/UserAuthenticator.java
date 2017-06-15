package com.jvm_bloggers.frontend.admin_area.login;

import org.apache.wicket.authroles.authorization.strategies.role.Roles;
import org.springframework.stereotype.Service;

@Service
public class UserAuthenticator {

    public static final String JASYPT_PROPERTY_KEY = "jasypt.encryptor.password";

    public Roles getRolesForUser(String login, String password) {
        String jasyptPassword =
            System.getProperty(JASYPT_PROPERTY_KEY, System.getenv(JASYPT_PROPERTY_KEY));

        if (jasyptPassword.equals(password)) {
            return new Roles(Roles.ADMIN);
        } else {
            return new Roles(Roles.USER);
        }
    }
}
