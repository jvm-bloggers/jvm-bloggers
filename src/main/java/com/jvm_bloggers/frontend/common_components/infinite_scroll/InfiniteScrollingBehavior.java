package com.jvm_bloggers.frontend.common_components.infinite_scroll;

import com.fasterxml.jackson.databind.ObjectMapper;

import javaslang.control.Try;
import lombok.extern.slf4j.Slf4j;

import org.apache.wicket.Component;
import org.apache.wicket.behavior.Behavior;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.JavaScriptHeaderItem;
import org.apache.wicket.markup.head.OnLoadHeaderItem;
import org.apache.wicket.request.resource.ResourceReference;
import org.apache.wicket.resource.JQueryPluginResourceReference;

import java.util.HashMap;
import java.util.Map;

@Slf4j
public class InfiniteScrollingBehavior extends Behavior {

    private static final ObjectMapper objectMapper = new ObjectMapper();
    private static final ResourceReference
        JS = new JQueryPluginResourceReference(InfiniteScrollingBehavior.class,
        "js/jquery.infinitescroll.js");

    private boolean autoScroll = true;
    private final Map<String, Object> jsonData;

    public InfiniteScrollingBehavior() {
        jsonData = new HashMap<>();
        jsonData.put("localMode", true);
        jsonData.put("animate", true);
    }

    @Override
    public void renderHead(Component component, IHeaderResponse headerResponse) {
        super.renderHead(component, headerResponse);
        headerResponse.render(JavaScriptHeaderItem.forReference(JS));
        headerResponse.render(OnLoadHeaderItem.forScript(createScript(component)));
    }

    protected CharSequence createScript(Component component) {
        String jsonConfig = Try.of(() -> objectMapper.writeValueAsString(jsonData))
            .onFailure(throwable -> log.error(throwable.getMessage()))
            .getOrElse("{}");
        CharSequence script = "jQuery( document ).ready(function() { jQuery('#"
            + component.getMarkupId() + "').infinitescroll("
            + jsonConfig + ",function(a){}); });";
        if (!autoScroll) {
            script = script + "jQuery(window).unbind('.infscr');";
            script = script + "jQuery('" + jsonData.get("nextSelector") + "').click(function() {"
                + "            jQuery(document).trigger('retrieve.infscr');"
                + "            return false;"
                + "       });";
        }
        return script;
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

    public InfiniteScrollingBehavior setNavSelector(Component component) {
        component.setOutputMarkupId(true);
        jsonData.put("navSelector", "#" + component.getMarkupId(true));
        return this;
    }

    public InfiniteScrollingBehavior setItemSelector(Component component, String selector) {
        component.setOutputMarkupId(true);
        jsonData.put("itemSelector", "#" + component.getMarkupId() + " " + selector);
        return this;
    }

    public InfiniteScrollingBehavior debug(boolean debug) {
        jsonData.put("debug", debug);
        return this;
    }

    public InfiniteScrollingBehavior loadingFinishedMsg(String finishedMsg) {
        getLoadingData().put("finishedMsg", finishedMsg);
        return this;
    }

    public InfiniteScrollingBehavior loadingMsgText(String msgText) {
        getLoadingData().put("msgText", msgText);
        return this;
    }

    public InfiniteScrollingBehavior extraScrollPx(Integer extraScrollPx) {
        getLoadingData().put("extraScrollPx", extraScrollPx);
        return this;
    }

    public InfiniteScrollingBehavior bufferPx(Integer bufferPx) {
        getLoadingData().put("bufferPx", bufferPx);
        return this;
    }

    public InfiniteScrollingBehavior loadingImg(String img) {
        getLoadingData().put("img", img);
        return this;
    }

    private Map<String, Object> getLoadingData() {
        Map<String, Object> loading = (Map<String, Object>) jsonData.get("loading");
        if (loading == null) {
            loading = new HashMap<>();
            jsonData.put("loading", loading);
        }
        return loading;
    }
}
