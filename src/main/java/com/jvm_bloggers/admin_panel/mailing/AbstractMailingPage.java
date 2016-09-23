package com.jvm_bloggers.admin_panel.mailing;

import com.jvm_bloggers.admin_panel.AbstractAdminPage;

import org.apache.wicket.markup.html.link.BookmarkablePageLink;

public abstract class AbstractMailingPage extends AbstractAdminPage {
    public AbstractMailingPage() {
        initializeElements();
    }

    private void initializeElements() {
        initializeTabPanel();
    }

    private void initializeTabPanel() {
        add(new BookmarkablePageLink<>("mailingTemplateLink", MailingPage.class));
        add(new BookmarkablePageLink<>("mailingAddressesLink", MailingAddressPage.class));
    }
}
