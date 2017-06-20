package com.jvm_bloggers.frontend.public_area.social_meta_data;

import org.apache.wicket.markup.head.IHeaderResponse;

import static org.apache.wicket.markup.head.MetaDataHeaderItem.forMetaTag;

public class SocialMetaDataHeadRenderer {

    private final SocialMetaData socialMetaTags;

    public SocialMetaDataHeadRenderer(SocialMetaData socialMetaTags) {
        this.socialMetaTags = socialMetaTags;
    }

    public void renderTo(IHeaderResponse response) {
        response.render(forMetaTag("og:title", socialMetaTags.getTitle()));
        response.render(forMetaTag("og:description", socialMetaTags.getDescription()));
        response.render(forMetaTag("og:image", socialMetaTags.getImageUrl()));

        response.render(forMetaTag("twitter:title", socialMetaTags.getTitle()));
        response.render(forMetaTag("twitter:image:alt", socialMetaTags.getTitle()));
        response.render(forMetaTag("twitter:description", socialMetaTags.getDescription()));
        response.render(forMetaTag("twitter:image", socialMetaTags.getImageUrl()));
    }
}
