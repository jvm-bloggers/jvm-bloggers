package com.jvm_bloggers.frontend.admin_area.login

import org.apache.wicket.authroles.authorization.strategies.role.Roles
import spock.lang.Specification
import spock.lang.Subject

class UserAuthenticatorSpec extends Specification {

    static String PASSWORD = "secretPassword";

    @Subject
    UserAuthenticator authenticator = new UserAuthenticator()

    def setupSpec() {
        System.setProperty("jasypt.encryptor.password", PASSWORD);
    }

    def "Should return ADMIN role if password is correct"() {
        given:
        String password = PASSWORD

        when:
        Roles roles = authenticator.getRolesForUser("any", password)

        then:
        roles.hasRole(Roles.ADMIN)
        !roles.hasRole(Roles.USER)
    }

    def "Should return USER role if password is invalid"() {
        given:
        String password = "incorrect"

        when:
        Roles roles = authenticator.getRolesForUser("any", password)

        then:
        roles.hasRole(Roles.USER)
        !roles.hasRole(Roles.ADMIN)
    }
}
