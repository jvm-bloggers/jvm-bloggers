package pl.tomaszdziurko.jvm_bloggers.view.admin.mailing;

import org.apache.wicket.authroles.authorization.strategies.role.Roles;
import org.apache.wicket.authroles.authorization.strategies.role.annotations.AuthorizeInstantiation;
import pl.tomaszdziurko.jvm_bloggers.view.admin.BaseAdminPage;

@AuthorizeInstantiation(Roles.ADMIN)
public class MailingPage extends BaseAdminPage {
}
