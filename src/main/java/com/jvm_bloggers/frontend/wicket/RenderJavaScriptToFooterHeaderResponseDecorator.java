package com.jvm_bloggers.frontend.wicket;

import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.filter.JavaScriptFilteredIntoFooterHeaderResponse;
import org.apache.wicket.markup.html.DecoratingHeaderResponse;
import org.apache.wicket.markup.html.IHeaderResponseDecorator;

public class RenderJavaScriptToFooterHeaderResponseDecorator extends DecoratingHeaderResponse {

    private String bucketName;

    public RenderJavaScriptToFooterHeaderResponseDecorator(String bucketName) {
        super();
        this.bucketName = bucketName;
    }

    @Override
    public IHeaderResponse decorate(IHeaderResponse response) {
        return new JavaScriptFilteredIntoFooterHeaderResponse(response, bucketName);
    }
}
