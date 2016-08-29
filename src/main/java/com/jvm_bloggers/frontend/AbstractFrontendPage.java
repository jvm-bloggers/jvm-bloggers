package com.jvm_bloggers.frontend;

import com.jvm_bloggers.frontend.common_layout.FooterFrontend;
import com.jvm_bloggers.frontend.common_layout.HeaderFrontend;
import com.jvm_bloggers.frontend.common_layout.RightFrontendSidebar;

import org.apache.wicket.Component;
import org.apache.wicket.markup.html.WebPage;
import org.apache.wicket.request.mapper.parameter.PageParameters;

public abstract class AbstractFrontendPage extends WebPage {

    public AbstractFrontendPage() {
        this(null);
    }

    public AbstractFrontendPage(PageParameters parameters) {
        super(parameters);

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

}
