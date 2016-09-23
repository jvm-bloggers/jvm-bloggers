package com.jvm_bloggers.admin_panel.session;

import lombok.extern.slf4j.Slf4j;

import org.apache.wicket.Session;
import org.apache.wicket.authroles.authentication.AbstractAuthenticatedWebSession;
import org.apache.wicket.authroles.authorization.strategies.role.Roles;
import org.apache.wicket.request.Request;

@Slf4j
public class UserSession extends AbstractAuthenticatedWebSession {

    public static final String GUEST_ROLE = "GUEST";

    private String name;
    private Roles roles;

    public UserSession(Request request) {
        super(request);
        roles = new Roles(GUEST_ROLE);
        name = "Guest";
    }

    public static UserSession get() {
        return (UserSession) Session.get();
    }

    @Override
    public Roles getRoles() {
        return roles;
    }

    @Override
    public boolean isSignedIn() {
        return roles.hasRole(Roles.ADMIN);
    }

    public void loginAs(String name, Roles roles) {
        this.name = name;
        this.roles = roles;
        log.info("Logging as " + name + " with roles = " + roles);
    }

    public String getName() {
        return name;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("User{");
        sb.append("name='").append(name).append('\'');
        sb.append(", roles=").append(roles);
        sb.append('}');
        return sb.toString();
    }

}
