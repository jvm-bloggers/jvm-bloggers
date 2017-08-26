package com.jvm_bloggers.frontend.admin_area;

import com.jvm_bloggers.frontend.admin_area.blogs.BlogsPage;
import com.jvm_bloggers.frontend.admin_area.mailing.MailingPage;
import com.jvm_bloggers.frontend.admin_area.moderation.ModerationPage;

import org.apache.wicket.Component;
import org.apache.wicket.authroles.authorization.strategies.role.Roles;
import org.apache.wicket.authroles.authorization.strategies.role.annotations.AuthorizeInstantiation;
import org.apache.wicket.devutils.debugbar.DebugBar;
import org.apache.wicket.markup.head.filter.HeaderResponseContainer;
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
        add(createFooterContainer());
    }

    private void initializeElements() {
        initializeLeftPanel();
    }

    private void initializeLeftPanel() {
        add(new BookmarkablePageLink<>("adminHomePage", AdminDashboardPage.class));
        add(new BookmarkablePageLink<>("dashboardLink", AdminDashboardPage.class));
        add(new BookmarkablePageLink<>("adminSocialChannelsPage", AdminSocialChannelsPage.class));
        add(new BookmarkablePageLink<>("mailingLink", MailingPage.class));
        add(new BookmarkablePageLink<>("moderationLink", ModerationPage.class));
        add(new BookmarkablePageLink<>("blogsLink", BlogsPage.class));
        add(new DebugBar("debug"));
    }

    private Component createFooterContainer() {
        return new HeaderResponseContainer("footer-container", "footer-container");
    }
}
