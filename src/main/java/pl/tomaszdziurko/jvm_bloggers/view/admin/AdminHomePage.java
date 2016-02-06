package pl.tomaszdziurko.jvm_bloggers.view.admin;

import org.apache.wicket.authroles.authorization.strategies.role.Roles;
import org.apache.wicket.authroles.authorization.strategies.role.annotations.AuthorizeInstantiation;
import pl.tomaszdziurko.jvm_bloggers.view.BasePage;

@AuthorizeInstantiation(Roles.ADMIN)
public class AdminHomePage extends BasePage {
}
