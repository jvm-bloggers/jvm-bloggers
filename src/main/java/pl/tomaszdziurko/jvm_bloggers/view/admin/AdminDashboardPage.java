package pl.tomaszdziurko.jvm_bloggers.view.admin;

import org.apache.wicket.authroles.authorization.strategies.role.Roles;
import org.apache.wicket.authroles.authorization.strategies.role.annotations.AuthorizeInstantiation;
import org.apache.wicket.markup.html.basic.Label;
import pl.tomaszdziurko.jvm_bloggers.view.admin.counters.NewPostsCounterModel;

@AuthorizeInstantiation(Roles.ADMIN)
public class AdminDashboardPage extends AbstractAdminPage {

    public AdminDashboardPage() {
        Label postsSinceLastPublicationLabel = new Label("postsSinceLastPublication", new NewPostsCounterModel());
        add(postsSinceLastPublicationLabel);
    }
}
