package com.jvm_bloggers.frontend.wicket;

import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.filter.JavaScriptFilteredIntoFooterHeaderResponse;
import org.apache.wicket.markup.html.DecoratingHeaderResponse;

public class RenderJavaScriptToFooterHeaderResponseDecorator extends DecoratingHeaderResponse {

    private String bucketName;

    public RenderJavaScriptToFooterHeaderResponseDecorator(String bucketName, IHeaderResponse response) {
        super(response);
        this.bucketName = bucketName;
    }

    public IHeaderResponse decorate(IHeaderResponse response) {
        return new JavaScriptFilteredIntoFooterHeaderResponse(response, bucketName);
    }
}
