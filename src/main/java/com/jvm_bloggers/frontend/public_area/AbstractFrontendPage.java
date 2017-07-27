package com.jvm_bloggers.frontend.public_area;

import com.jvm_bloggers.frontend.public_area.common_layout.FooterFrontend;
import com.jvm_bloggers.frontend.public_area.common_layout.HeaderFrontend;
import com.jvm_bloggers.frontend.public_area.common_layout.RightFrontendSidebar;
import com.jvm_bloggers.frontend.public_area.social_meta_data.SocialMetaData;
import com.jvm_bloggers.frontend.public_area.social_meta_data.SocialMetaDataHeadRenderer;
import org.apache.wicket.Component;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.filter.HeaderResponseContainer;
import org.apache.wicket.markup.html.WebPage;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.request.mapper.parameter.PageParameters;

import static com.jvm_bloggers.frontend.public_area.social_meta_data.DefaultSocialMetaData.getInstance;
import static java.lang.String.format;

public abstract class AbstractFrontendPage extends WebPage {

    static final String TITLE_PREFIX = "JVM Bloggers";

    public AbstractFrontendPage() {
        this(null);
    }

    public AbstractFrontendPage(PageParameters parameters) {
        super(parameters);
        add(new Label("pageTitle", preparePageTitle()));
        add(createHeader());
        add(createRightSidebar());
        add(createFooter());
        add(createFooterContainer());
    }

    private Component createHeader() {
        return new HeaderFrontend("headerFrontend");
    }

    private Component createRightSidebar() {
        return new RightFrontendSidebar("rightSidebar");
    }

    private Component createFooter() {
        return new FooterFrontend("footer");
    }

    private String preparePageTitle() {
        return format("%s - %s", TITLE_PREFIX, getPageTitle());
    }

    @Override
    public void renderHead(IHeaderResponse response) {
        super.renderHead(response);
        new SocialMetaDataHeadRenderer(getSocialMetaTags()).renderTo(response);
    }

    protected abstract String getPageTitle();

    protected SocialMetaData getSocialMetaTags() {
        return getInstance();
    }

    private Component createFooterContainer() {
        return new HeaderResponseContainer("footer-container", "footer-container");
    }
}
