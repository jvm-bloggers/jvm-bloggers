package com.jvm_bloggers.frontend.public_area.social_meta_data;

import lombok.Getter;
import lombok.ToString;

@Getter
@ToString
public class DefaultSocialMetaData implements SocialMetaData {

    public static final String DEFAULT_TITLE = "JVM Bloggers - wszystkie polskie blogi"
        + " o ekosytemie Javy i programowaniu";
    public static final String DEFAULT_DESCRIPTION = "Wszystkie blogi o Javie, ekosystemie JVM"
        + " i programowaniu pisane przez polskich programistów i polskie firmy w jednym miejscu,"
        + " co tydzień nowe wydanie, co tydzień nowa porcja wiedzy";
    public static final String DEFAULT_IMAGE = "http://jvm-bloggers.com/jvm-bloggers.png";

    private static final DefaultSocialMetaData INSTANCE = new DefaultSocialMetaData();

    private String title;
    private String description;
    private String imageUrl;

    public static DefaultSocialMetaData getInstance() {
        return INSTANCE;
    }

    protected DefaultSocialMetaData() {
        this(DEFAULT_TITLE, DEFAULT_DESCRIPTION, DEFAULT_IMAGE);
    }

    protected DefaultSocialMetaData(String title, String description, String imageUrl) {
        this.title = title;
        this.description = description;
        this.imageUrl = imageUrl;
    }

    public DefaultSocialMetaData withTitle(String title) {
        return new DefaultSocialMetaData(title, getDescription(), getImageUrl());
    }

}
