package com.jvm_bloggers.admin_panel;

import com.jvm_bloggers.admin_panel.blogs.BlogsPage;
import com.jvm_bloggers.admin_panel.mailing.MailingPage;
import com.jvm_bloggers.admin_panel.moderation.ModerationPage;

import org.apache.wicket.authroles.authorization.strategies.role.Roles;
import org.apache.wicket.authroles.authorization.strategies.role.annotations.AuthorizeInstantiation;
import org.apache.wicket.devutils.debugbar.DebugBar;
import org.apache.wicket.markup.html.WebPage;
import org.apache.wicket.markup.html.link.BookmarkablePageLink;
import org.apache.wicket.spring.injection.annot.SpringBean;

@AuthorizeInstantiation(Roles.ADMIN)
public abstract class AbstractAdminPage extends WebPage {

    @SpringBean
    protected PaginationConfiguration paginationConfiguration;

    protected final int defaultPaginationSize;

    public AbstractAdminPage() {
        defaultPaginationSize = paginationConfiguration.getDefaultPageSize();
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
