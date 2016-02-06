package pl.tomaszdziurko.jvm_bloggers.view.admin;

import org.apache.wicket.authroles.authorization.strategies.role.Roles;
import org.apache.wicket.authroles.authorization.strategies.role.annotations.AuthorizeInstantiation;

@AuthorizeInstantiation(Roles.ADMIN)
public class AdminDashboardPage extends BaseAdminPage {
}
