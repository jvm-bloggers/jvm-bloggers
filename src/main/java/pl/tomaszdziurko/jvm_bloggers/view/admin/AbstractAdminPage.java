package pl.tomaszdziurko.jvm_bloggers.view.admin;

import org.apache.wicket.devutils.debugbar.DebugBar;
import org.apache.wicket.markup.html.WebPage;
import org.apache.wicket.markup.html.link.BookmarkablePageLink;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import pl.tomaszdziurko.jvm_bloggers.view.admin.mailing.MailingPage;
import pl.tomaszdziurko.jvm_bloggers.view.admin.moderation.ModerationPage;

public abstract class AbstractAdminPage extends WebPage {

    public AbstractAdminPage() {
        initializeElements();
    }

    public AbstractAdminPage(PageParameters parameters) {
        super(parameters);
        initializeElements();
    }

    private void initializeElements() {
        initializeLeftPanel();
    }

    private void initializeLeftPanel() {
        add(new BookmarkablePageLink<AdminDashboardPage>("adminHomePage", AdminDashboardPage.class));
        add(new BookmarkablePageLink<AdminDashboardPage>("dashboardLink", AdminDashboardPage.class));
        add(new BookmarkablePageLink<AdminDashboardPage>("mailingLink", MailingPage.class));
        add(new BookmarkablePageLink<AdminDashboardPage>("moderationLink", ModerationPage.class));

        add(new DebugBar("debug"));    }
}
