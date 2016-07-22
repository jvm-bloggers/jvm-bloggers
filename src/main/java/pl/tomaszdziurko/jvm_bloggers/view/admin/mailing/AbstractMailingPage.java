package pl.tomaszdziurko.jvm_bloggers.view.admin.mailing;

import org.apache.wicket.markup.html.link.BookmarkablePageLink;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import pl.tomaszdziurko.jvm_bloggers.view.admin.AbstractAdminPage;

public abstract class AbstractMailingPage extends AbstractAdminPage {
    public AbstractMailingPage() {
        initializeElements();
    }

    public AbstractMailingPage(PageParameters parameters) {
        super(parameters);
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
