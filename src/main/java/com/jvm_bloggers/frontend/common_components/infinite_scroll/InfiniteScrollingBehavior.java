package com.jvm_bloggers.frontend.common_components.infinite_scroll;

import de.agilecoders.wicket.webjars.request.resource.WebjarsJavaScriptResourceReference;

import lombok.extern.slf4j.Slf4j;

import org.apache.wicket.Component;
import org.apache.wicket.behavior.Behavior;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.JavaScriptHeaderItem;
import org.apache.wicket.request.resource.JavaScriptResourceReference;
import org.apache.wicket.request.resource.ResourceReference;

import java.util.HashMap;
import java.util.Map;

import static java.lang.String.format;

@Slf4j
public class InfiniteScrollingBehavior extends Behavior {

    private static final String INIT_JS_ID = "InitInfiniteScroll";

    private static final ResourceReference
        AJAX_INTERCEPTOR = new JavaScriptResourceReference(InfiniteScrollingBehavior.class,
            "js/ajax-request-interceptor.js");

    private static final ResourceReference
        JS = new WebjarsJavaScriptResourceReference(
            "infinite-scroll/current/dist/infinite-scroll.pkgd.min.js");

    private static final ResourceReference
        CUSTOM_JS = new JavaScriptResourceReference(InfiniteScrollingBehavior.class,
        "js/infinite-scroll-event-listener.js");

    private final Map<String, Object> jsonData;

    public InfiniteScrollingBehavior() {
        jsonData = new HashMap<>();
        jsonData.put("debug", false);
    }

    @Override
    public void renderHead(Component component, IHeaderResponse headerResponse) {
        super.renderHead(component, headerResponse);
        headerResponse.render(JavaScriptHeaderItem.forReference(AJAX_INTERCEPTOR));
        headerResponse.render(JavaScriptHeaderItem.forReference(JS));
        headerResponse.render(JavaScriptHeaderItem.forScript(createScript(component), INIT_JS_ID));
        headerResponse.render(JavaScriptHeaderItem.forReference(CUSTOM_JS));
    }

    private CharSequence createScript(Component component) {
        return format(""
            + "var $infiniteScrollContainer = jQuery('#%s').infiniteScroll({\n"
            + "  path: function() {\n"
            + "    return document.querySelector('%s')\n"
            + "                   .getAttribute('href');\n"
            + "  },\n"
            + "  append: '.item',\n"
            + "  responseType: 'text',\n"
            + "  debug: %s\n"
            + "});", component.getMarkupId(), jsonData.get("nextSelector"), jsonData.get("debug"));
    }

    @Override
    public void bind(Component component) {
        super.bind(component);
        component.setOutputMarkupId(true);
    }

    public InfiniteScrollingBehavior setNextSelector(Component component) {
        component.setOutputMarkupId(true);
        jsonData.put("nextSelector", "#" + component.getMarkupId(true));
        return this;
    }

    public InfiniteScrollingBehavior debug(boolean debug) {
        jsonData.put("debug", debug);
        return this;
    }
}
