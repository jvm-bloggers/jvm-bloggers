package com.jvm_bloggers.frontend.common_components.toastr;

import de.agilecoders.wicket.webjars.request.resource.WebjarsCssResourceReference;
import de.agilecoders.wicket.webjars.request.resource.WebjarsJavaScriptResourceReference;
import org.apache.wicket.Component;
import org.apache.wicket.behavior.Behavior;
import org.apache.wicket.markup.head.CssHeaderItem;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.JavaScriptHeaderItem;

public class ToastrBehavior extends Behavior {

    private static final String TOASTR_CSS = "toastr/current/build/toastr.min.css";
    private static final String TOASTR_JS = "toastr/current/build/toastr.min.js";

    @Override
    public void renderHead(Component component, IHeaderResponse response) {
        super.renderHead(component, response);
        response.render(CssHeaderItem.forReference(
                new WebjarsCssResourceReference(TOASTR_CSS)));
        response.render(JavaScriptHeaderItem.forReference(
                new WebjarsJavaScriptResourceReference(TOASTR_JS)));
    }
}
