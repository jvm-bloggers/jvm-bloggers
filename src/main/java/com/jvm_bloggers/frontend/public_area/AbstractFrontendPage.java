package com.jvm_bloggers.frontend.public_area;

import com.jvm_bloggers.frontend.public_area.common_layout.FooterFrontend;
import com.jvm_bloggers.frontend.public_area.common_layout.HeaderFrontend;
import com.jvm_bloggers.frontend.public_area.common_layout.RightFrontendSidebar;
import org.apache.wicket.Component;
import org.apache.wicket.markup.html.WebPage;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.request.mapper.parameter.PageParameters;

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

    protected abstract String getPageTitle();

}
