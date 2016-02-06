package pl.tomaszdziurko.jvm_bloggers.view.login

import org.apache.wicket.authroles.authorization.strategies.role.Roles
import spock.lang.Specification
import spock.lang.Subject

class UserAuthenticatorSpec extends Specification {

    @Subject
    UserAuthenticator authenticator = new UserAuthenticator();

    def "Should return ADMIN role if password is correct"() {
        given:
            String password = "password"
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
