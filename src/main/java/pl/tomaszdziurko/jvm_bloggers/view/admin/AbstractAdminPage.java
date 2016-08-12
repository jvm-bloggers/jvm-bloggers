package pl.tomaszdziurko.jvm_bloggers.view.admin;

import org.apache.wicket.devutils.debugbar.DebugBar;
import org.apache.wicket.markup.html.WebPage;
import org.apache.wicket.markup.html.link.BookmarkablePageLink;

import pl.tomaszdziurko.jvm_bloggers.JvmBloggersApplication;
import pl.tomaszdziurko.jvm_bloggers.view.admin.blogs.BlogsPage;
import pl.tomaszdziurko.jvm_bloggers.view.admin.mailing.MailingPage;
import pl.tomaszdziurko.jvm_bloggers.view.admin.moderation.ModerationPage;

public abstract class AbstractAdminPage extends WebPage {

    protected final int defaultPaginationSize;

    public AbstractAdminPage() {
        defaultPaginationSize = ((JvmBloggersApplication) getApplication()).getDefaultPaginationSize();
        initializeElements();
    }

    private void initializeElements() {
        initializeLeftPanel();
    }

    private void initializeLeftPanel() {
        add(new BookmarkablePageLink<>("adminHomePage", AdminDashboardPage.class));
        add(new BookmarkablePageLink<>("dashboardLink", AdminDashboardPage.class));
        add(new BookmarkablePageLink<>("mailingLink", MailingPage.class));
        add(new BookmarkablePageLink<>("moderationLink", ModerationPage.class));
        add(new BookmarkablePageLink<>("blogsLink", BlogsPage.class));
        add(new DebugBar("debug"));
    }
}
