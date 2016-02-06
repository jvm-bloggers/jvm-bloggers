package pl.tomaszdziurko.jvm_bloggers.view.admin;

import org.apache.wicket.markup.html.WebPage;
import org.apache.wicket.markup.html.link.BookmarkablePageLink;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import pl.tomaszdziurko.jvm_bloggers.view.admin.mailing.MailingPage;

public abstract class BaseAdminPage extends WebPage {

    public BaseAdminPage() {
        initializeElements();
    }

    public BaseAdminPage(PageParameters parameters) {
        super(parameters);
        initializeElements();
    }

    private void initializeElements() {
        initializeLeftPanel();
    }

    private void initializeLeftPanel() {
        add(new BookmarkablePageLink<AdminDashboardPage>("dashboardLink", AdminDashboardPage.class));
        add(new BookmarkablePageLink<AdminDashboardPage>("mailingLink", MailingPage.class));
    }
}
